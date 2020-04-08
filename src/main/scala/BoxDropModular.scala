
import chisel3._
import chisel3.util._

/**
  * FSM: This one should take care of issuing commands, but that's it
  * Should probably also be an intermediary that sends signals between the datapath(also taking care of colour outputs),
  * and the top-level device.
  */
class BDM_FSM extends Module {
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
  State logic
   */
  val cOp = Wire(Vec(6, Bool())) //Should default to false
  val cOpU = cOp.asUInt() //for debug purposes, makes it easier to inspect on waveforms
  io.op := cOp

  //Default assign everything to zeroes
  cOp := VecInit(Seq.fill(6)(false.B))

  //Set operand inputs based on current state
  when(stateReg === sMoveLR) {
    cOp(CoordCmds.right) := io.btnR //Right
    cOp(CoordCmds.left) := io.btnL //Left
    cOp(CoordCmds.flip) := io.btnU //Flip
  } .elsewhen(stateReg === sMoveDown) {
    cOp(CoordCmds.down) := io.btnD || frame63
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

/**
  * Datapath for the entire system!
  * When it gets an operand, a flag should go high (is this necessary?)
  * When it finishes an operation, it should respond with finished=true (forever or for just one clock cycle?)
  *
  */
class BoxDropModular(maxDepth: Int) extends Box {
  //Signals
  val x: Bits = (io.col-160.U) >> 5 //X coordinate on game grid
  val y: Bits = io.row >> 5 //Y coordinate on game grid
  io.x := x
  io.y := y

  /*
  MODULES AND ASSIGNMENTS
   */
  val FSM = Module(new BDM_FSM())
  FSM.io.btnR := io.btnR
  FSM.io.btnL := io.btnL
  FSM.io.btnU := io.btnU
  FSM.io.btnD := io.btnD
  FSM.io.frame := io.frame
  val op = FSM.io.op
  val en = FSM.io.en
  val fin = FSM.io.finished
  val validDrop = FSM.io.validDrop
  validDrop := true.B

  val mem = Module(new MemoryGrid())
  mem.io.wrData := false.B
  mem.io.wen := false.B
  mem.io.ren := false.B
  mem.io.X := false.B
  mem.io.Y := false.B

  val rand = Module(new Rand())
  when(io.btnR || io.btnU || io.btnL || io.btnD) {
    rand.io.up := 1.U
  } .otherwise {
    rand.io.up := 0.U
  }




  val coords: Vec[Coord] = Reg(Vec(4, new Coord))
  val c0 = coords(0)
  val c1 = coords(1)
  val c2 = coords(2)
  val c3 = coords(3)
  io.coords := coords

  //Defaults
  fin := false.B

  //Display output logic
  when(io.vblank) {
    setColours(0.U, 0.U, 0.U)
  } .otherwise {
    drawBoxes()
  }

  //The actual datapath
  //When enabled and valid data is ready for us
  when(en) {
    //moveLR status
    //Left/right/flip are mutually exclusive, default to prioritizing right
    when(op(CoordCmds.right)) { //Move right
      moveRight()
    }.elsewhen(op(CoordCmds.left)) { //Move left
      moveLeft()
    }.elsewhen(op(CoordCmds.flip)) {
      doFlip()
    }

    //Sequential when-statements are used to allow for moving and dropping in the same update frame
    when(op(CoordCmds.down)) { //Move down
      moveDown()
    }

    //Saving to ram
    when(op(CoordCmds.savePiece)) {
      saveToRAM()
    }

    //Adding new pieces
   when(op(CoordCmds.addNew)) {
     val g: UInt = rand.io.out % 7.U
     when(g === 0.U) {
       addLeftSquiggly()
     } .elsewhen(g === 1.U) {
       addRightSquiggly()
     } .elsewhen(g === 2.U) {
       addBar()
     } .elsewhen(g === 3.U) {
       addCube()
     } .elsewhen(g === 4.U) { //When g===4.U
       addT()
     } .elsewhen(g === 5.U) {
       addLeftL()
     } .otherwise { //when g === 6.U
       addRightL()
     }
     fin:=true.B
   }
  }

  def moveDown(): Unit = {
    //Collisions with bottom of screen
    val t = Wire(Vec(4, Bool()))
    for(i <- 0 to 3) {
      t(i) := coords(i).y === maxDepth.U
    }
    val bottomCollision = (t(0) || t(1) || t(2) || t(3))

    val rdCnt = RegInit(7.U(3.W)) //Init to 7, signals that no reads are currently happening
    //Start counting when we enter this loop
    when(rising(op(CoordCmds.down))) {
      rdCnt := 0.U
    }

    when(rdCnt < 4.U) {
      when(read(coords(rdCnt).x, coords(rdCnt).y + 1.U)) { //If we're reading something below us
        fin := true.B
        validDrop := false.B
        rdCnt := 7.U
      } .otherwise {
        rdCnt := rdCnt + 1.U
      }
    } .elsewhen(rdCnt === 4.U) {
      rdCnt := 7.U
      when(bottomCollision) {
        //This requires us to write 4 values into RAM, requires 4 sequential writes
        fin := true.B
        validDrop := false.B
      } .otherwise {
        addY(1)
        fin:= true.B
        validDrop := true.B
      }
    }
  }

  def moveLeft(): Unit = {
    val movCnt = RegInit(7.U(3.W))
    //Check if any x-coord is already 0
    val t = Wire(Vec(4, Bool()))
    for (i <- 0 to 3) {
      t(i) := coords(i).x =/= 0.U
    }
    
    //Check memory positions for existing pieces

    when(rising(en)) {
      movCnt := 0.U
    }
    when(movCnt < 4.U) {
      when(read( (coords(movCnt).x - 1.U), coords(movCnt).y)) { //if a piece already exists, don't try to move into place
        movCnt := 7.U
        fin := true.B
      } .otherwise {
        movCnt := movCnt + 1.U
      }
    } .elsewhen(movCnt === 4.U) {
      //Check all memory positions
      when(t(0) && t(1) && t(2) && t(3)) {
        subX(1)
      }
      movCnt := 7.U
      fin := true.B
    }
  }


  def moveRight(): Unit = {
    val movCnt = RegInit(7.U(3.W))

    //Check if any x-coord is already 9
    val t = Wire(Vec(4, Bool()))
    for (i <- 0 to 3) {
      t(i) := coords(i).x =/= 9.U
    }
    //Check memory positions for existing pieces

    when(rising(en)) {
      movCnt := 0.U
    }
    when(movCnt < 4.U) {
      when(read(coords(movCnt).x + 1.U, coords(movCnt).y)) { //if a piece already exists, don't try to move into place
        movCnt := 7.U
        fin := true.B
      } .otherwise {
        movCnt := movCnt + 1.U
      }
    } .elsewhen(movCnt === 4.U) {
      //Check all memory positions
      when(t(0) && t(1) && t(2) && t(3)) {
        addX(1)
      }
      movCnt := 7.U
      fin := true.B
    }
  }

  def doFlip(): Unit = {
    //Do a flip!
    fin := true.B
  }

  def saveToRAM(): Unit = {
    val saveCnt = RegInit(7.U(3.W))
    //When entering, set it to 0
    when(rising(op(CoordCmds.savePiece))) {
      saveCnt := 0.U
    }
    //Loop through the pieces
    when(saveCnt < 4.U) {
      write(coords(saveCnt).x, coords(saveCnt).y, true.B)
      saveCnt := saveCnt + 1.U
    } .elsewhen(saveCnt === 4.U) { //Once we hit the end, set cnt back to 7, mark that we're finished
      fin := true.B
      saveCnt := 7.U
    }
  }


  /**
    * Performs a read from the memory position corresponding to field (x,y)
    * @param x The x coordinate of the field to check
    * @param y The y coordinate of the field to check
    * @return true if the field is set, false otherwise
    */
  def read(x: UInt, y: UInt): Bool = {
    mem.io.wen := false.B
    mem.io.ren := true.B
    mem.io.X := x
    mem.io.Y := y
    mem.io.rdData
  }

  /**
    * Writes a value into the memory position corresponding to field (x,y)
    * @param x The x coordinate of the field to write
    * @param y The y coordinate of the field to write
    * @param d The value to write to this field
    */
  def write(x: UInt, y: UInt, d: Bool):Unit = {
    mem.io.wen := true.B
    mem.io.ren := false.B
    mem.io.X := x
    mem.io.Y := y
    mem.io.wrData := d
  }



  /**
    * Draws all boxes on the game screen
    */
  def drawBoxes(): Unit = {
    val t = Wire(Vec(4, Bool()))
    for(i <- 0 to 3) {
      t(i) := (x.asUInt() === coords(i).x && y.asUInt() === coords(i).y)
    }

    when(160.U <= io.col && io.col < 480.U)  {//Middle half of the screen
      when(t(0) || t(1) || t(2) || t(3)) { //Currently dropping block
        setColours(0.U, 10.U, 0.U)
      } .elsewhen(read(x.asUInt(), y.asUInt())) { //In memory
        setColours(15.U, 0.U, 0.U)
      } .otherwise { //Rest of the game area
        setColours(0.U, 7.U, 7.U)
      }
    } otherwise { //outer 1/4 on each side
      setColours(3.U, 3.U, 3.U)
    }
  }

  def addX(v: Int): Unit = {
    c0.x := c0.x + v.U
    c1.x := c1.x + v.U
    c2.x := c2.x + v.U
    c3.x := c3.x + v.U
  }
  def subX(v: Int): Unit = {
    c0.x := c0.x - v.U
    c1.x := c1.x - v.U
    c2.x := c2.x - v.U
    c3.x := c3.x - v.U
  }
  def addY(v: Int): Unit = {
    c0.y := c0.y + v.U
    c1.y := c1.y + v.U
    c2.y := c2.y + v.U
    c3.y := c3.y + v.U
  }

  def addLeftSquiggly(): Unit = {
    c0.x := 5.U
    c0.y := 0.U

    c1.x := 5.U
    c1.y := 1.U

    c2.x := 4.U
    c2.y := 1.U

    c3.x := 4.U
    c3.y := 2.U
  }
  def addRightSquiggly(): Unit = {
    c0.x := 5.U
    c0.y := 0.U

    c1.x := 5.U
    c1.y := 1.U

    c2.x := 6.U
    c2.y := 1.U

    c3.x := 6.U
    c3.y := 2.U
  }
  def addBar(): Unit = {
    c0.x := 5.U
    c0.y := 0.U

    c1.x := 5.U
    c1.y := 1.U

    c2.x := 5.U
    c2.y := 2.U

    c3.x := 5.U
    c3.y := 3.U
  }
  def addCube(): Unit = {
    c0.x := 4.U
    c0.y := 0.U

    c1.x := 5.U
    c1.y := 0.U

    c2.x := 4.U
    c2.y := 1.U

    c3.x := 5.U
    c3.y := 1.U
  }
  def addT(): Unit = {
    c0.x := 5.U
    c0.y := 0.U

    c1.x := 5.U
    c1.y := 1.U

    c2.x := 5.U
    c2.y := 2.U

    c3.x := 6.U
    c3.y := 1.U
  }

  def addLeftL(): Unit = {
    c0.x := 5.U
    c0.y := 0.U

    c1.x := 5.U
    c1.y := 1.U

    c2.x := 5.U
    c2.y := 2.U

    c3.x := 4.U
    c3.y := 2.U
  }

  def addRightL(): Unit = {
    c0.x := 5.U
    c0.y := 0.U

    c1.x := 5.U
    c1.y := 1.U

    c2.x := 5.U
    c2.y := 2.U

    c3.x := 6.U
    c3.y := 2.U
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

object BoxDropModular extends App {
  val a = Array("--target-dir", "output")
  chisel3.Driver.execute(a, () => new BoxDropModular(maxDepth = 14))
}