
import chisel3._
import chisel3.util._

class Playground extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val finished = Output(Bool())
  })

  val cm = Module(new CheckMem)
  val mem = Module(new MemoryGrid(testing=true.B))

  cm.io.mem <> mem.io

//  cm.io.mem.rdData := mem.io.rdData
//  mem.io.X := cm.io.mem.X
//  mem.io.Y := cm.io.mem.Y
//  mem.io.wrData := cm.io.mem.wrData
//  mem.io.ren := cm.io.mem.ren
//  mem.io.wen := cm.io.mem.wen

  cm.io.en := io.en
  io.finished := cm.io.finished
}

class memIO extends Bundle {
  val X = Output(UInt(4.W))
  val Y = Output(UInt(4.W))
  val wrData = Output(Bool())
  val wen = Output(Bool())
  val ren = Output(Bool())
  val rdData = Input(Bool())
}

class CheckMem extends Module {
  val io = IO(new Bundle {
    val mem = new memIO()
    val en = Input(Bool())
    val finished = Output(Bool())
  })


  //Instantiating values used later on
  val xCoord = RegInit(9.U(4.W))
  val yCoord = RegInit(14.U(4.W)) //Should actually start at 14
  val rowCnt = RegInit(0.U(3.W))
  val rowsToRemove = Reg(Vec(4, UInt(4.W)))
  val rdData = WireDefault(false.B) //This is used to get the currently read value
  val rdReg = RegNext(rdData) //This is used to obtain the value read on the previous clock cycle
  val nextLine = !rdData || (xCoord === 0.U)

  //These two values are used in reading/writing sections
  val rw_x = xCoord
  val rw_y = rowsToRemove(rowCnt-1.U) //Using (-1) to get correct value.
    //If eq rowsToRemove=1, we need to select the element at index 0

  //Default assignments
  io.finished := false.B
  io.mem.ren := false.B
  io.mem.wen := false.B
  io.mem.X := xCoord
  io.mem.Y := yCoord
  io.mem.wrData := false.B


  //State register
  val idle::checking::transition::reading::writing::finished::Nil = Enum(6)
  val state = RegInit(idle);

  switch(state) {
    is(idle) {
//      rdData := read(xCoord, yCoord) // To prepare the system, we need to have data ready
      when(io.en) {
        state := checking
      }
    }

    is(checking) {
      rdData := read(xCoord, yCoord)
      when(rdData) {
        when(xCoord > 0.U) {
          xCoord := xCoord - 1.U
        } .otherwise { //xcoord = 0
          rowsToRemove(rowCnt) := yCoord
          rowCnt := rowCnt + 1.U
        }
      }

      when(nextLine) { //Row finished or false value read
        xCoord := 9.U
        yCoord := yCoord - 1.U
      }
      when(nextLine && (yCoord === 0.U)) {
        state := reading
        yCoord := 14.U
      }
    }

    is(reading) { //When entering this, xCoord should already be equal to 9
      val fin = (rowCnt === 0.U)
      when(fin) {
        state := finished
      } .otherwise {
        state := writing
        rdData := read(rw_x, rw_y-1.U) //Reading at y-1 to read the row above
      }
    }

    is(writing) {

      write(rw_x, rw_y, rdReg) //We need to write using the value obtained in previous clock cycle
      when(xCoord > 0.U) {
        xCoord := xCoord - 1.U
      } .otherwise { //xcoord = 0, row finished
        xCoord := 9.U
        rw_y := rw_y - 1.U
      }

      when(rw_x === 0.U && rw_y === 1.U) { //Finished moving all blocks
        rowCnt := rowCnt - 1.U
      }
      state := reading
    }

    is(finished) {
      io.finished := true.B
      xCoord := 9.U
      yCoord := 15.U
      when(!io.en) {
        state := idle
      }
    }
  }
  //Memory checker

  def rising(v: Bool): Bool = v && !RegNext(v)

  def read(x: UInt, y: UInt): Bool = {
    io.mem.wen := false.B
    io.mem.ren := true.B
    io.mem.X := x
    io.mem.Y := y
    io.mem.rdData
  }

  def write(x: UInt, y: UInt, d: Bool):Unit = {
    io.mem.wen := true.B
    io.mem.ren := false.B
    io.mem.X := x
    io.mem.Y := y
    io.mem.wrData := d
  }
}

object CheckMem extends App {
  val a = Array("--target-dir", "output")
  chisel3.Driver.execute(a, () => new CheckMem())
}


