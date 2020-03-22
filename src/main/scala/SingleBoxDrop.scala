
import chisel3._
import chisel3.util._

class SingleBoxDrop extends Box {
  /**
    * Sets the coords of current block to a new squiggly at (5,0), (5,1), (4,1), (4,2)
    */
  def addNewBlock():Unit = {
    coords(0).x := 0.U
    coords(0).y := 0.U

    coords(1).x := 0.U
    coords(1).y:= 1.U

    coords(2).x := 1.U
    coords(2).y := 1.U

    coords(3).x := 1.U
    coords(3).y := 2.U

    state := sCheckbelow
    running := false.B
  }

  /**
    * Updates the position of the currently falling block.
    * If it detects a collision, writes the currently falling block into memory
    */
  def checkBlockPositions(): Unit = {
    val cnt = RegInit(0.U(3.W))
    //Loop over the 4 pieces of our currently falling block. Check if, for any of them, the piece directly
    //below in the memory contains anything
    when(cnt < 4.U) {
      //Read the grid position just below each field of current block
      when(read(coords(cnt).x, coords(cnt).y + 1.U) || coords(cnt).y === 14.U) {
        //If grid position just below us = 1 OR any block is in the 14th layer, stop moving down
        state := sWriteram
        running := false.B
        cnt := 0.U
      } otherwise {
        cnt := cnt + 1.U
      }
    } otherwise {
      cnt := 0.U
      running := false.B
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

  /**
    * Writes the current piece into RAM at its positions
    * Afterwards, sets flags such that next cycle will add a new piece to the screen
    */
  def writePieceToRAM(): Unit = {
    val cnt = RegInit(0.U(3.W))
    when(cnt < 4.U) {
      write(coords(cnt).x, coords(cnt).y, true.B)
      cnt := cnt + 1.U
    } otherwise {
      cnt := 0.U
      running := false.B
      state := sAddnew
    }
  }

  /**
    * Draws all boxes on the screen during the display period
    */
  def drawBoxes(): Unit = {

    //Either of these statements will be true when current coords equal the falling block
    val t = Wire(Vec(4, Bool()))
    for(i <- 0 to 3) {
      t(i) := (x.asUInt() === coords(i).x && y.asUInt() === coords(i).y)
    }

    when(160.U <= io.col && io.col < 480.U)  {//Middle half of the screen
      when(t(0) || t(1) || t(2) || t(3)) {
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
  //Create memory
  val mem: MemoryGrid = Module(new MemoryGrid)
  //We don't need to force any of these guys to have values
  mem.io.wen := DontCare
  mem.io.ren := DontCare
  mem.io.X := DontCare
  mem.io.Y := DontCare
  mem.io.wrData := DontCare


  val coords = Reg(Vec(4, new Coord))
  io.coords := coords
  //Do we need to initalize these?

/*  val x: UInt = Wire(UInt())
  val y: UInt = Wire(UInt())
  x := (io.col - 160.U) >> 5
  io.x := x
  y := io.row >> 5
  io.y := y*/
  val x = (io.col-160.U) >> 5 //division by 32
  val y = io.row >> 5 //division by 32
  io.x := x
  io.y := y

  //Frame counter. Update every 64 frames
  val frameCnt: UInt = RegInit(0.U(6.W))
  val frameTick: Bool = frameCnt === "b111111".U
  frameCnt := Mux(io.frame, frameCnt + 1.U, frameCnt)

  //Control variables
  val running: Bool = RegInit(false.B)
  /*
  val addNewPiece: Bool = RegInit(true.B)
  val pieceIsMoving: Bool = RegInit(false.B)
  val addPieceToRAM: Bool = RegInit(false.B)
  val legalMove: Bool = RegInit(false.B)*/

  val sAddnew::sCheckbelow::sDropdown::sWriteram::Nil = Enum(4)
  val state: UInt = RegInit(sAddnew)

  //Control flow
  when(rising(frameTick)) {
    running := true.B
  }

  when(!io.vblank) {  //When in drawing mode
    drawBoxes()
  } .elsewhen(io.vblank && running) { //During blanking interval and on an update frame
    setColours(0.U, 0.U, 0.U)
    frameCnt := 0.U
    switch(state) {
      is(sAddnew) {
        addNewBlock()
      }
      is(sCheckbelow) {
        checkBlockPositions()
      }
      is(sDropdown) {
        dropDown()
      }
      is(sWriteram) {
        writePieceToRAM()
      }
    }
  } .otherwise {
    setColours(0.U, 0.U, 0.U)
  }
}

object SingleBoxDrop extends App {
  chisel3.Driver.execute(args, () => new SingleBoxDrop())
}
