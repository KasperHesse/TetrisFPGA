
import chisel3._
import chisel3.util._

class SingleBoxDrop extends Box {

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
      t(i) := ((x.asUInt() === coords(i).x) && (y.asUInt() === coords(i).y))
    }

    when(160.U <= io.col && io.col < 480.U)  {//Middle half of the screen
      /*when(read(x.asUInt(), y.asUInt())) {
        setColours(15.U, 0.U, 0.U)
      } .else*/when(t(0) || t(1) || t(2) || t(3)) {
        setColours(0.U, 10.U, 0.U)
      } .elsewhen(read(x.asUInt(), y.asUInt())) {
        setColours(15.U, 0.U, 0.U)
      } .otherwise {
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
}

object SingleBoxDrop extends App {
  val a = Array("--target-dir", "output")
  chisel3.Driver.execute(a, () => new SingleBoxDrop())
}
