
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
    val op: Vec[Bool] = Output(Vec(5, Bool()))
  })
  def rising(v: Bool): Bool = v && !RegNext(v)
  /*
  FSM states and setup
   */
  val sMoveLR::sMoveDown::sAddNew::sStop::Nil = Enum(4)
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
  val cOp = Wire(Vec(5, Bool())) //Should default to false
  io.op := cOp
  //Set operand inputs based on current state
  when(stateReg === sMoveLR) {
    cOp(CoordCmds.down) := false.B
    cOp(CoordCmds.right) := io.btnR //Right
    cOp(CoordCmds.left) := io.btnL //Left
    cOp(CoordCmds.flip) := io.btnU //Flip
    cOp(CoordCmds.addNew) := false.B
  } .elsewhen(stateReg === sMoveDown) {
    cOp(CoordCmds.down) := io.btnD || frame63
    cOp(CoordCmds.right) := false.B //Right
    cOp(CoordCmds.left) := false.B //Left
    cOp(CoordCmds.flip) := false.B //Flip
    cOp(CoordCmds.addNew) := false.B
  } .elsewhen(stateReg === sAddNew) {
    cOp(CoordCmds.down) := false.B
    cOp(CoordCmds.right) := false.B //Right
    cOp(CoordCmds.left) := false.B //Left
    cOp(CoordCmds.flip) := false.B //Flip
    cOp(CoordCmds.addNew) := true.B
  } .otherwise {
    cOp := VecInit(Seq.fill(5)(false.B))
  }

  when(running) {
    switch(stateReg) {
      is(sMoveLR) {
        stateReg := sMoveDown
      }
      is(sMoveDown) {
        when(io.finished) {
          when(io.validDrop) {
            stateReg := sMoveLR
            running := false.B
          }.elsewhen(!io.validDrop) {
            stateReg := sAddNew
          }
        } .elsewhen(!cOp(CoordCmds.down)) { //If not finished, it might be because we're not supposed to be here at all!
          stateReg := sMoveLR
          running := false.B
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
class BoxDropModular extends Box {
  val x: Bits = (io.col-160.U) >> 5 //X coordinate on game grid
  val y: Bits = io.row >> 5 //Y coordinate on game grid
  io.x := x
  io.y := y


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
  def addBbar(): Unit = {
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
      //Check if any x-coord is already 9
      val t = Wire(Vec(4, Bool()))
      for (i <- 0 to 3) {
        t(i) := coords(i).x =/= 9.U
      }
      when(t(0) && t(1) && t(2) && t(3)) {
        addX(1)
      }
      fin := true.B
    }.elsewhen(op(CoordCmds.left)) { //Move left
      //Check if any x-coord is already 0
      val t = Wire(Vec(4, Bool()))
      for (i <- 0 to 3) {
        t(i) := coords(i).x =/= 0.U
      }
      when(t(0) && t(1) && t(2) && t(3)) {
        subX(1)
      }
      fin := true.B
    }.elsewhen(op(CoordCmds.flip)) {
      //Do a flip!
      fin := true.B
    }

    //Sequential when-statements are used to allow for moving and dropping in the same update frame
    when(op(CoordCmds.down)) { //Move down
      //TODO Enable collision checking
      addY(1)
      fin:= true.B
      validDrop := true.B
    }

    //Adding new pieces
   when(op(CoordCmds.addNew)) {
     //Add some random-number generation stuff here?
     addLeftSquiggly()
     fin:=true.B
   }
  }
}

object CoordCmds {
  val down = 0.U
  val right = 1.U
  val left = 2.U
  val flip = 3.U
  val addNew = 4.U
}

object BoxDropModular extends App {
  val a = Array("--target-dir", "output")
  chisel3.Driver.execute(a, () => new BoxDropModular())
}

/**

/**
  * Sets the coords of current block to a new squiggly at (5,0), (5,1), (4,1), (4,2)
  */
  def addNewBlock():Unit = {
    coords(0).x := 5.U
    coords(0).y := 0.U

    coords(1).x := 5.U
    coords(1).y:= 1.U

    coords(2).x := 4.U
    coords(2).y := 1.U

    coords(3).x := 4.U
    coords(3).y := 2.U

    state := sCheckbelow
    running := false.B
  }

  val checkEn: Bool = RegInit(false.B)
  val checkCnt: UInt = RegInit(7.U(3.W))
  /**
  * Checks whether the currently falling block can move further down
  * Sets next state depending on the outcome
  */
  def checkBlockPositions(): Unit = {
    //Located outside for debugging purposes

    //Enable-signal is used for determinining state transitions
    checkEn := (state === sCheckbelow)
    when(rising(checkEn)) {
      checkCnt := 0.U
    }
    //Loop over the 4 pieces of our currently falling block. Check if, for any of them, the piece directly
    //below in the memory contains anything
    when(checkCnt < 4.U) {
      //Read the grid position just below each field of current block
      when(read(coords(checkCnt).x, coords(checkCnt).y + 1.U) || coords(checkCnt).y === 14.U) {
        //If grid position just below us = 1 OR any block is in the 14th layer, stop moving down
        checkEn := false.B
        checkCnt := 6.U //Set count to 4 to indicate that we're finished
        state := sWriteram
      }

      checkCnt := checkCnt + 1.U
    } .elsewhen(checkEn && checkCnt === 4.U) { //Move forward as planned
      checkEn := false.B
      checkCnt := 6.U //Set to 7 to avoid timing issues on next round
      state := sDropdown
    }
  }

  /**
  * Drops the current piece down by 1 field by incrementing all y-values
  */
  def dropDown(): Unit = {
    coords(0).y := coords(0).y + 1.U
    coords(1).y := coords(1).y + 1.U
    coords(2).y := coords(2).y + 1.U
    coords(3).y := coords(3).y + 1.U

    state := sCheckbelow
    running := false.B
  }

  val RAMen: Bool = RegInit(false.B)
  val RAMcnt: UInt = RegInit(7.U(3.W))
  /**
  * Writes the current piece into RAM at its positions
  * Afterwards, sets flags such that next cycle will add a new piece to the screen
  */
  def writePieceToRAM(): Unit = {

    RAMen := (state === sWriteram)
    when(rising(RAMen)) {
      RAMcnt := 0.U
    }

    when(RAMcnt < 4.U) {
      write(coords(RAMcnt).x, coords(RAMcnt).y, true.B)
      RAMcnt := RAMcnt + 1.U
    } .elsewhen(RAMen && RAMcnt === 4.U) {
      state := sAddnew
      RAMcnt := 7.U
      RAMen := false.B
    }
  }

  /**
  * Draws all boxes on the screen during the display period
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
      } .otherwise { //Rest of the screen
        setColours(0.U, 7.U, 7.U)
      }
    } otherwise { //outer 1/4 on each side
      setColours(3.U, 3.U, 3.U)
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

  /// IMPLEMENTATION ///
  val running: Bool = RegInit(false.B) //High during updates in blanking interval
  val sAddnew::sCheckbelow::sDropdown::sWriteram::Nil = Enum(4) //States
  val state: UInt = RegInit(sAddnew) //State register
  //Create memory
  val mem: MemoryGrid = Module(new MemoryGrid)
  //By default, don't do anything
  mem.io.wen := false.B
  mem.io.ren := false.B
  mem.io.X := 0.U
  mem.io.Y := 0.U
  mem.io.wrData := false.B


  val coords: Vec[Coord] = Reg(Vec(4, new Coord)) //Coordinates of the currently falling block
  io.coords := coords

  val x: Bits = (io.col-160.U) >> 5 //X coordinate on game grid
  val y: Bits = io.row >> 5 //Y coordinate on game grid
  io.x := x
  io.y := y

  //Frame counter. Update every 64 frames
  val frameCnt: UInt = RegInit(0.U(6.W))
  val frameTick: Bool = frameCnt === "b111111".U
  frameCnt := Mux(io.frame, frameCnt + 1.U, frameCnt)

  //Movement counter. Allow movement every 16 frames
  val moveCnt: UInt = RegInit(0.U(4.W))
  moveCnt := Mux(io.frame && moveCnt < 15.U, moveCnt + 1.U, moveCnt)


  //Control flow
  when(rising(frameTick)) { //Every 64 frames (approx 1 second) drop the boxes
    running := true.B
    frameCnt := 0.U
  }

  when(!io.vblank) {  //When in drawing mode
    drawBoxes()
  } .elsewhen(io.vblank && running) { //During blanking interval and on an update frame
    setColours(0.U, 0.U, 0.U)
    switch(state) {
      is(sAddnew) {
        addNewBlock()
      }
      is(sCheckbelow) {
        checkBlockPositions()
        //        moveBoxes()
      }
      is(sDropdown) {
        dropDown()
        //        moveBoxes()
      }
      is(sWriteram) {
        writePieceToRAM()
      }
    }

  } .otherwise {
    setColours(0.U, 0.U, 0.U) //Default assignment
  }
  */