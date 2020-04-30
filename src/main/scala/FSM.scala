import chisel3._
import chisel3.util._

/**
  * FSM: This one should take care of issuing commands, but that's it
  * Should probably also be an intermediary that sends signals between the datapath(also taking care of colour outputs),
  * and the top-level device.
  */
class FSM extends Module {
  val io = IO(new Bundle {
    val finished: Bool = Input(Bool()) //Signals that the DP is finished processing data
    val validDrop: Bool = Input(Bool()) //Signals whether a drop operation was valid or not

    val frame: Bool = Input(Bool())
    val btnR: Bool = Input(Bool())
    val btnL: Bool = Input(Bool())
    val btnD: Bool = Input(Bool())
    val btnU: Bool = Input(Bool())
    val en: Bool = Output(Bool())
    val op: Vec[Bool] = Output(Vec(6, Bool()))
  })
  def rising(v: Bool): Bool = v && !RegNext(v)

  /*
  FSM states and setup
   */
  val sMoveLR::sMoveDown::sAddNew::sSavePiece::sIdle::Nil = Enum(5)
  val stateReg: UInt = RegInit(sAddNew)

  /*
  Framecnt and frame values
   */
  val frameCnt: UInt = RegInit(0.U(6.W))
  frameCnt := Mux(io.frame, frameCnt + 1.U, frameCnt)
  val frame31: Bool = (frameCnt === "b011111".U) //Move, flip frame
  val frame63: Bool = (frameCnt === "b111111".U) //Downwards collision frame
  val running: Bool = RegInit(false.B) //Are we currently processing anything?
  io.en := running

  //Set running=true when either frame31 or frame63 is hit (twice per second, approx)
  when(rising(frame31 || frame63)) {
    running := true.B
  }
  /*
  Command vector sent to datapath
   */
  val cOp = Wire(Vec(6, Bool())) //Commands for DP, one-hot encoded according to CoordCmds.
  val cOpU = cOp.asUInt() //for debug purposes, makes it easier to inspect on waveforms
  io.op := cOp

    /*
  State logic
   */
  cOp := VecInit(Seq.fill(6)(false.B)) //All operands default to false
  //Set operand inputs based on current state
  when(stateReg === sMoveLR) {
    cOp(CoordCmds.right) := io.btnR
    cOp(CoordCmds.left) := io.btnL
    cOp(CoordCmds.flip) := io.btnU
  } .elsewhen(stateReg === sMoveDown) {
    cOp(CoordCmds.down) := io.btnD || frame63 //Always move down on frame63, may also move down on btnD presses
  } .elsewhen(stateReg === sAddNew) {
    cOp(CoordCmds.addNew) := true.B
  } .elsewhen(stateReg === sSavePiece) {
    cOp(CoordCmds.savePiece) := true.B
  }

  when(running) {
    switch(stateReg) {
      is(sMoveLR) {
        when(io.finished) {
          stateReg := sMoveDown
        } .elsewhen(!io.btnR && !io.btnU && !io.btnL) {
          stateReg := sMoveDown
        }
      }
      is(sMoveDown) {
        when(io.finished) {
          when(io.validDrop) {
            stateReg := sMoveLR
            running := false.B
          }.elsewhen(!io.validDrop) {
            stateReg := sSavePiece
          }
        } .elsewhen(!cOp(CoordCmds.down)) { //If not finished, it might be because we're not supposed to be here at all!
          stateReg := sMoveLR
          running := false.B
        }
      }
      is(sSavePiece) {
        //Wait for the saving procedure to finish
        when(io.finished) {
          stateReg := sAddNew
        }
      }
      is(sAddNew) {
        when(io.finished) {
          stateReg := sMoveLR
          running := false.B
          frameCnt := 0.U //Just to make sure
        }
      }
    }
  }
}


object CoordCmds {
  val down = 0.U
  val right = 1.U
  val left = 2.U
  val flip = 3.U
  val addNew = 4.U
  val savePiece = 5.U
}