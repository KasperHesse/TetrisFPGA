
import chisel3._
import chisel3.util._

/**
  * Datapath for the entire system
  *
  */
class BoxDropModular(maxDepth: Int = 14) extends Box {
  //Signals
  val x: Bits = (io.col-160.U) >> 5 //X coordinate on game grid
  val y: Bits = io.row >> 5 //Y coordinate on game grid
  io.x := x
  io.y := y

  /*
  ===============MODULES AND DEFAULT ASSIGNMENTS================
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
  mem.io.X := 0.U
  mem.io.Y := 0.U

  val rand = Module(new Rand())
  when(io.btnR || io.btnU || io.btnL || io.btnD) { //Whenever a button is pressed, count up
    rand.io.up := 1.U
  } .otherwise {
    rand.io.up := 0.U
  }

  /*
  ========== COORDINATES AND ROTATION STATE
   */
  //Rotation management
  val rotation: UInt = RegInit(0.U(2.W))
  val co: Vec[Vec[CoordOffsets]] = Wire(Vec(7, Vec(4, new CoordOffsets)))
  val leftS::rightS::bar::cube::t::leftL::rightL::Nil = Enum(7)
  //Coordinates
  val coords: Vec[Coord] = Wire(Vec(4, new Coord))
  val baseX: UInt = RegInit(0.U(4.W))
  val baseY: UInt = RegInit(0.U(4.W))
  val piece: UInt = RegInit(leftS)

  val c0 = coords(0)
  val c1 = coords(1)
  val c2 = coords(2)
  val c3 = coords(3)
  io.coords := coords

  //Set coordinates based on offsets from base coordinate
  c0.x := baseX + co(piece)(rotation).x0
  c0.y := baseY + co(piece)(rotation).y0
  c1.x := baseX + co(piece)(rotation).x1
  c1.y := baseY + co(piece)(rotation).y1
  c2.x := baseX + co(piece)(rotation).x2
  c2.y := baseY + co(piece)(rotation).y2
  c3.x := baseX + co(piece)(rotation).x3
  c3.y := baseY + co(piece)(rotation).y3

  icm2()
  //Outer parameter: Block
  //Middle parameter: Rotation state (0=up, 1=right, 2=down, 3=left)
  //Inner parameter: Pieces
/*
  def instantiateCoordMods(): Unit = {

    /*
    Coordinate modifiers for left squiggly
     */
    co(leftS)(0).x0 := 0.S
    co(leftS)(0).y0 := 0.S
    co(leftS)(0).x1 := 0.S
    co(leftS)(0).y1 := (-1).S
    co(leftS)(0).x2 := (-1).S
    co(leftS)(0).y2 := 0.S
    co(leftS)(0).x3 := (-1).S
    co(leftS)(0).y3 := 1.S

    co(leftS)(1).x0 := 0.S
    co(leftS)(1).y0 := 0.S
    co(leftS)(1).x1 := 1.S
    co(leftS)(1).y1 := 0.S
    co(leftS)(1).x2 := 0.S
    co(leftS)(1).y2 := (-1).S
    co(leftS)(1).x3 := (-1).S
    co(leftS)(1).y3 := (-1).S

    co(leftS)(2).x0 := 0.S
    co(leftS)(2).y0 := 0.S
    co(leftS)(2).x1 := 0.S
    co(leftS)(2).y1 := (-1).S
    co(leftS)(2).x2 := (-1).S
    co(leftS)(2).y2 := 0.S
    co(leftS)(2).x3 := (-1).S
    co(leftS)(2).y3 := 1.S

    co(leftS)(3).x0 := 0.S
    co(leftS)(3).y0 := 0.S
    co(leftS)(3).x1 := 1.S
    co(leftS)(3).y1 := 0.S
    co(leftS)(3).x2 := 0.S
    co(leftS)(3).y2 := (-1).S
    co(leftS)(3).x3 := (-1).S
    co(leftS)(3).y3 := (-1).S

    /*
    Coordinate modifiers for right squiggly
     */
    co(rightS)(0).x0 := 0.S
    co(rightS)(0).y0 := 0.S
    co(rightS)(0).x1 := 0.S
    co(rightS)(0).y1 := (-1).S
    co(rightS)(0).x2 := 1.S
    co(rightS)(0).y2 := 0.S
    co(rightS)(0).x3 := 1.S
    co(rightS)(0).y3 := 1.S

    co(rightS)(1).x0 := 0.S
    co(rightS)(1).y0 := 0.S
    co(rightS)(1).x1 := 1.S
    co(rightS)(1).y1 := 0.S
    co(rightS)(1).x2 := 0.S
    co(rightS)(1).y2 := 1.S
    co(rightS)(1).x3 := (-1).S
    co(rightS)(1).y3 := 1.S

    co(rightS)(2).x0 := 0.S
    co(rightS)(2).y0 := 0.S
    co(rightS)(2).x1 := 0.S
    co(rightS)(2).y1 := (-1).S
    co(rightS)(2).x2 := 1.S
    co(rightS)(2).y2 := 0.S
    co(rightS)(2).x3 := 1.S
    co(rightS)(2).y3 := 1.S

    co(rightS)(3).x0 := 0.S
    co(rightS)(3).y0 := 0.S
    co(rightS)(3).x1 := 1.S
    co(rightS)(3).y1 := 0.S
    co(rightS)(3).x2 := 0.S
    co(rightS)(3).y2 := 1.S
    co(rightS)(3).x3 := (-1).S
    co(rightS)(3).y3 := 1.S

    /*
    Coordinate modifiers for bar
     */
    co(bar)(0).x0 := 0.S
    co(bar)(0).y0 := 0.S
    co(bar)(0).x1 := 0.S
    co(bar)(0).y1 := (-1).S
    co(bar)(0).x2 := 0.S
    co(bar)(0).y2 := 1.S
    co(bar)(0).x3 := 0.S
    co(bar)(0).y3 := 2.S

    co(bar)(1).x0 := 0.S
    co(bar)(1).y0 := 1.S
    co(bar)(1).x1 := 1.S
    co(bar)(1).y1 := 1.S
    co(bar)(1).x2 := (-1).S
    co(bar)(1).y2 := 1.S
    co(bar)(1).x3 := (-2).S
    co(bar)(1).y3 := 1.S

    co(bar)(2).x0 := 0.S
    co(bar)(2).y0 := 0.S
    co(bar)(2).x1 := 0.S
    co(bar)(2).y1 := (-1).S
    co(bar)(2).x2 := 0.S
    co(bar)(2).y2 := 1.S
    co(bar)(2).x3 := 0.S
    co(bar)(2).y3 := 2.S

    co(bar)(3).x0 := 0.S
    co(bar)(3).y0 := 1.S
    co(bar)(3).x1 := 1.S
    co(bar)(3).y1 := 1.S
    co(bar)(3).x2 := (-1).S
    co(bar)(3).y2 := 1.S
    co(bar)(3).x3 := (-2).S
    co(bar)(3).y3 := 1.S

    /*
    Coordinate modifiers for cube
     */
    co(cube)(0).x0 := 0.S
    co(cube)(0).y0 := 0.S
    co(cube)(0).x1 := 1.S
    co(cube)(0).y1 := 0.S
    co(cube)(0).x2 := 0.S
    co(cube)(0).y2 := 1.S
    co(cube)(0).x3 := 1.S
    co(cube)(0).y3 := 1.S

    co(cube)(1).x0 := 0.S
    co(cube)(1).y0 := 0.S
    co(cube)(1).x1 := 1.S
    co(cube)(1).y1 := 0.S
    co(cube)(1).x2 := 0.S
    co(cube)(1).y2 := 1.S
    co(cube)(1).x3 := 1.S
    co(cube)(1).y3 := 1.S

    co(cube)(2).x0 := 0.S
    co(cube)(2).y0 := 0.S
    co(cube)(2).x1 := 1.S
    co(cube)(2).y1 := 0.S
    co(cube)(2).x2 := 0.S
    co(cube)(2).y2 := 1.S
    co(cube)(2).x3 := 1.S
    co(cube)(2).y3 := 1.S

    co(cube)(3).x0 := 0.S
    co(cube)(3).y0 := 0.S
    co(cube)(3).x1 := 1.S
    co(cube)(3).y1 := 0.S
    co(cube)(3).x2 := 0.S
    co(cube)(3).y2 := 1.S
    co(cube)(3).x3 := 1.S
    co(cube)(3).y3 := 1.S

    co(t)(0).x0 := 0.S
    co(t)(0).y0 := 0.S
    co(t)(0).x1 := 0.S
    co(t)(0).y1 := (-1).S
    co(t)(0).x2 := 0.S
    co(t)(0).y2 := 1.S
    co(t)(0).x3 := 1.S
    co(t)(0).y3 := 0.S

    co(t)(1).x0 := 0.S
    co(t)(1).y0 := 0.S
    co(t)(1).x1 := 1.S
    co(t)(1).y1 := 0.S
    co(t)(1).x2 := (-1).S
    co(t)(1).y2 := 0.S
    co(t)(1).x3 := 0.S
    co(t)(1).y3 := 1.S

    co(t)(2).x0 := 0.S
    co(t)(2).y0 := 0.S
    co(t)(2).x1 := 0.S
    co(t)(2).y1 := 1.S
    co(t)(2).x2 := 0.S
    co(t)(2).y2 := (-1).S
    co(t)(2).x3 := (-1).S
    co(t)(2).y3 := 0.S

    co(t)(3).x0 := 0.S
    co(t)(3).y0 := 0.S
    co(t)(3).x1 := (-1).S
    co(t)(3).y1 := 0.S
    co(t)(3).x2 := 1.S
    co(t)(3).y2 := 0.S
    co(t)(3).x3 := 0.S
    co(t)(3).y3 := (-1).S

    co(leftL)(0).x0 := 0.S
    co(leftL)(0).y0 := 0.S
    co(leftL)(0).x1 := 0.S
    co(leftL)(0).y1 := (-1).S
    co(leftL)(0).x2 := 0.S
    co(leftL)(0).y2 := 1.S
    co(leftL)(0).x3 := (-1).S
    co(leftL)(0).y3 := 1.S

    co(leftL)(1).x0 := 0.S
    co(leftL)(1).y0 := 0.S
    co(leftL)(1).x1 := 1.S
    co(leftL)(1).y1 := 0.S
    co(leftL)(1).x2 := (-1).S
    co(leftL)(1).y2 := 0.S
    co(leftL)(1).x3 := (-1).S
    co(leftL)(1).y3 := (-1).S

    co(leftL)(2).x0 := 0.S
    co(leftL)(2).y0 := 0.S
    co(leftL)(2).x1 := 0.S
    co(leftL)(2).y1 := 1.S
    co(leftL)(2).x2 := 0.S
    co(leftL)(2).y2 := (-1).S
    co(leftL)(2).x3 := 1.S
    co(leftL)(2).y3 := (-1).S

    co(leftL)(3).x0 := 0.S
    co(leftL)(3).y0 := 0.S
    co(leftL)(3).x1 := (-1).S
    co(leftL)(3).y1 := 0.S
    co(leftL)(3).x2 := 1.S
    co(leftL)(3).y2 := 0.S
    co(leftL)(3).x3 := 1.S
    co(leftL)(3).y3 := 1.S

    co(rightL)(0).x0 := 0.S
    co(rightL)(0).y0 := 0.S
    co(rightL)(0).x1 := 0.S
    co(rightL)(0).y1 := (-1).S
    co(rightL)(0).x2 := 0.S
    co(rightL)(0).y2 := 1.S
    co(rightL)(0).x3 := 1.S
    co(rightL)(0).y3 := 1.S

    co(rightL)(1).x0 := 0.S
    co(rightL)(1).y0 := 0.S
    co(rightL)(1).x1 := 1.S
    co(rightL)(1).y1 := 0.S
    co(rightL)(1).x2 := (-1).S
    co(rightL)(1).y2 := 0.S
    co(rightL)(1).x3 := (-1).S
    co(rightL)(1).y3 := 1.S

    co(rightL)(2).x0 := 0.S
    co(rightL)(2).y0 := 0.S
    co(rightL)(2).x1 := 0.S
    co(rightL)(2).y1 := 1.S
    co(rightL)(2).x2 := 0.S
    co(rightL)(2).y2 := (-1).S
    co(rightL)(2).x3 := (-1).S
    co(rightL)(2).y3 := (-1).S

    co(rightL)(3).x0 := 0.S
    co(rightL)(3).y0 := 0.S
    co(rightL)(3).x1 := (-1).S
    co(rightL)(3).y1 := 0.S
    co(rightL)(3).x2 := 1.S
    co(rightL)(3).y2 := 0.S
    co(rightL)(3).x3 := 1.S
    co(rightL)(3).y3 := (-1).S
  }
*/

  //Defaults
  fin := false.B

  //Display output logic
  when(io.vblank) {
    setColours(0.U, 0.U, 0.U)
  } .otherwise {
    drawBoxes()
  }


  //The datapath
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
     val g: UInt = rand.io.out % 7.U //Just a helper signal to determine which piece to add
     piece := g
     //Reset base coords
     baseX := 4.U
     baseY := 0.U
     /*
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
     }*/
     fin:=true.B
   }
  }


  /*
  ==============HELPER FUNCTIONS ARE DEFINED BELOW===============
   */
  /**
    * Tries to move a piece down by one row.
    * If successful, returns gracefully.
    * If unsuccesfull, calls 'saveToRam' to save the piece onto the game field
    * @see BoxDropModular#saveToRam()
    */
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

  /**
    * Move a piece left, except when this would cause it to overlap with another piece
    */
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


  /**
    * Move a piece right, except if this would cause it to overlap with another piece
    */
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

  /**
    * Do a flip!
    */
  def doFlip(): Unit = {
    //Do a flip!
    rotation := rotation + 1.U
    fin := true.B
    //Coordinates are defined from a base coordinate and other coords are offsets from this
  }

  /**
    * Saves a piece to RAM once it hits another piece
    */
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
    }

    when(saveCnt === 4.U) { //Once we hit the end, set cnt back to 7, mark that we're finished
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
      t(i) := ((x.asUInt() === coords(i).x) && (y.asUInt() === coords(i).y))
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
    baseX := baseX + v.U
  }
  def subX(v: Int): Unit = {
    baseX := baseX - v.U
  }
  def addY(v: Int): Unit = {
    baseY := baseY + v.U
  }

  def addLeftSquiggly(): Unit = {
    baseX := 5.S
    baseY := 1.S
  }
  def addRightSquiggly(): Unit = {
    baseX := 5.S
    baseY := 1.S
  }
  def addBar(): Unit = {
    baseX := 5.S
    baseY := 1.S
  }
  def addCube(): Unit = {
    baseX := 5.S
    baseY := 0.S
  }
  def addT(): Unit = {
    baseX := 5.S
    baseY := 1.S
  }

  def addLeftL(): Unit = {
    baseX := 5.S
    baseY := 1.S
  }

  def addRightL(): Unit = {
    baseX := 5.S
    baseY := 1.S
  }


  def icm2(): Unit = {

    /*
    Coordinate modifiers for left squiggly
     */
    co(leftS)(0).x0 := 1.U
    co(leftS)(0).y0 := 0.U
    co(leftS)(0).x1 := 1.U
    co(leftS)(0).y1 := 1.U
    co(leftS)(0).x2 := 2.U
    co(leftS)(0).y2 := 1.U
    co(leftS)(0).x3 := 2.U
    co(leftS)(0).y3 := 2.U

    co(leftS)(1).x0 := 2.U
    co(leftS)(1).y0 := 1.U
    co(leftS)(1).x1 := 1.U
    co(leftS)(1).y1 := 1.U
    co(leftS)(1).x2 := 1.U
    co(leftS)(1).y2 := 2.U
    co(leftS)(1).x3 := 0.U
    co(leftS)(1).y3 := 2.U

    co(leftS)(2).x0 := 1.U
    co(leftS)(2).y0 := 0.U
    co(leftS)(2).x1 := 1.U
    co(leftS)(2).y1 := 1.U
    co(leftS)(2).x2 := 2.U
    co(leftS)(2).y2 := 1.U
    co(leftS)(2).x3 := 2.U
    co(leftS)(2).y3 := 2.U

    co(leftS)(3).x0 := 2.U
    co(leftS)(3).y0 := 1.U
    co(leftS)(3).x1 := 1.U
    co(leftS)(3).y1 := 1.U
    co(leftS)(3).x2 := 1.U
    co(leftS)(3).y2 := 2.U
    co(leftS)(3).x3 := 0.U
    co(leftS)(3).y3 := 2.U

    /*
    Coordinate modifiers for right squiggly
     */
    co(rightS)(0).x0 := 2.U
    co(rightS)(0).y0 := 0.U
    co(rightS)(0).x1 := 2.U
    co(rightS)(0).y1 := 1.U
    co(rightS)(0).x2 := 1.U
    co(rightS)(0).y2 := 1.U
    co(rightS)(0).x3 := 1.U
    co(rightS)(0).y3 := 2.U

    co(rightS)(1).x0 := 2.U
    co(rightS)(1).y0 := 2.U
    co(rightS)(1).x1 := 1.U
    co(rightS)(1).y1 := 2.U
    co(rightS)(1).x2 := 1.U
    co(rightS)(1).y2 := 1.U
    co(rightS)(1).x3 := 0.U
    co(rightS)(1).y3 := 1.U

    co(rightS)(2).x0 := 2.U
    co(rightS)(2).y0 := 0.U
    co(rightS)(2).x1 := 2.U
    co(rightS)(2).y1 := 1.U
    co(rightS)(2).x2 := 1.U
    co(rightS)(2).y2 := 1.U
    co(rightS)(2).x3 := 1.U
    co(rightS)(2).y3 := 2.U

    co(rightS)(3).x0 := 2.U
    co(rightS)(3).y0 := 2.U
    co(rightS)(3).x1 := 1.U
    co(rightS)(3).y1 := 2.U
    co(rightS)(3).x2 := 1.U
    co(rightS)(3).y2 := 1.U
    co(rightS)(3).x3 := 0.U
    co(rightS)(3).y3 := 1.U

    /*
    Coordinate modifiers for bar
     */
    co(bar)(0).x0 := 2.U
    co(bar)(0).y0 := 0.U
    co(bar)(0).x1 := 2.U
    co(bar)(0).y1 := 1.U
    co(bar)(0).x2 := 2.U
    co(bar)(0).y2 := 2.U
    co(bar)(0).x3 := 2.U
    co(bar)(0).y3 := 3.U

    co(bar)(1).x0 := 0.U
    co(bar)(1).y0 := 2.U
    co(bar)(1).x1 := 1.U
    co(bar)(1).y1 := 2.U
    co(bar)(1).x2 := 2.U
    co(bar)(1).y2 := 2.U
    co(bar)(1).x3 := 3.U
    co(bar)(1).y3 := 2.U

    co(bar)(2).x0 := 2.U
    co(bar)(2).y0 := 0.U
    co(bar)(2).x1 := 2.U
    co(bar)(2).y1 := 1.U
    co(bar)(2).x2 := 2.U
    co(bar)(2).y2 := 2.U
    co(bar)(2).x3 := 2.U
    co(bar)(2).y3 := 3.U

    co(bar)(3).x0 := 0.U
    co(bar)(3).y0 := 2.U
    co(bar)(3).x1 := 1.U
    co(bar)(3).y1 := 2.U
    co(bar)(3).x2 := 2.U
    co(bar)(3).y2 := 2.U
    co(bar)(3).x3 := 3.U
    co(bar)(3).y3 := 2.U

    /*
    Coordinate modifiers for cube
     */
    co(cube)(0).x0 := 1.U
    co(cube)(0).y0 := 0.U
    co(cube)(0).x1 := 1.U
    co(cube)(0).y1 := 1.U
    co(cube)(0).x2 := 2.U
    co(cube)(0).y2 := 1.U
    co(cube)(0).x3 := 2.U
    co(cube)(0).y3 := 0.U

    co(cube)(1).x0 := 1.U
    co(cube)(1).y0 := 0.U
    co(cube)(1).x1 := 1.U
    co(cube)(1).y1 := 1.U
    co(cube)(1).x2 := 2.U
    co(cube)(1).y2 := 1.U
    co(cube)(1).x3 := 2.U
    co(cube)(1).y3 := 0.U

    co(cube)(2).x0 := 1.U
    co(cube)(2).y0 := 0.U
    co(cube)(2).x1 := 1.U
    co(cube)(2).y1 := 1.U
    co(cube)(2).x2 := 2.U
    co(cube)(2).y2 := 1.U
    co(cube)(2).x3 := 2.U
    co(cube)(2).y3 := 0.U

    co(cube)(3).x0 := 1.U
    co(cube)(3).y0 := 0.U
    co(cube)(3).x1 := 1.U
    co(cube)(3).y1 := 1.U
    co(cube)(3).x2 := 2.U
    co(cube)(3).y2 := 1.U
    co(cube)(3).x3 := 2.U
    co(cube)(3).y3 := 0.U

    co(t)(0).x0 := 1.U
    co(t)(0).y0 := 2.U
    co(t)(0).x1 := 1.U
    co(t)(0).y1 := 1.U
    co(t)(0).x2 := 1.U
    co(t)(0).y2 := 0.U
    co(t)(0).x3 := 2.U
    co(t)(0).y3 := 1.U

    co(t)(1).x0 := 0.U
    co(t)(1).y0 := 1.U
    co(t)(1).x1 := 1.U
    co(t)(1).y1 := 1.U
    co(t)(1).x2 := 2.U
    co(t)(1).y2 := 1.U
    co(t)(1).x3 := 1.U
    co(t)(1).y3 := 2.U

    co(t)(2).x0 := 1.U
    co(t)(2).y0 := 2.U
    co(t)(2).x1 := 1.U
    co(t)(2).y1 := 1.U
    co(t)(2).x2 := 1.U
    co(t)(2).y2 := 0.U
    co(t)(2).x3 := 0.U
    co(t)(2).y3 := 1.U

    co(t)(3).x0 := 2.U
    co(t)(3).y0 := 1.U
    co(t)(3).x1 := 1.U
    co(t)(3).y1 := 1.U
    co(t)(3).x2 := 0.U
    co(t)(3).y2 := 1.U
    co(t)(3).x3 := 1.U
    co(t)(3).y3 := 0.U

    co(leftL)(0).x0 := 1.U
    co(leftL)(0).y0 := 0.U
    co(leftL)(0).x1 := 1.U
    co(leftL)(0).y1 := 1.U
    co(leftL)(0).x2 := 1.U
    co(leftL)(0).y2 := 2.U
    co(leftL)(0).x3 := 0.U
    co(leftL)(0).y3 := 2.U

    co(leftL)(1).x0 := 2.U
    co(leftL)(1).y0 := 1.U
    co(leftL)(1).x1 := 1.U
    co(leftL)(1).y1 := 1.U
    co(leftL)(1).x2 := 0.U
    co(leftL)(1).y2 := 1.U
    co(leftL)(1).x3 := 0.U
    co(leftL)(1).y3 := 0.U

    co(leftL)(2).x0 := 1.U
    co(leftL)(2).y0 := 2.U
    co(leftL)(2).x1 := 1.U
    co(leftL)(2).y1 := 1.U
    co(leftL)(2).x2 := 1.U
    co(leftL)(2).y2 := 0.U
    co(leftL)(2).x3 := 2.U
    co(leftL)(2).y3 := 0.U

    co(leftL)(3).x0 := 0.U
    co(leftL)(3).y0 := 1.U
    co(leftL)(3).x1 := 1.U
    co(leftL)(3).y1 := 1.U
    co(leftL)(3).x2 := 2.U
    co(leftL)(3).y2 := 1.U
    co(leftL)(3).x3 := 2.U
    co(leftL)(3).y3 := 2.U

    co(rightL)(0).x0 := 1.U
    co(rightL)(0).y0 := 0.U
    co(rightL)(0).x1 := 1.U
    co(rightL)(0).y1 := 1.U
    co(rightL)(0).x2 := 1.U
    co(rightL)(0).y2 := 2.U
    co(rightL)(0).x3 := 2.U
    co(rightL)(0).y3 := 2.U

    co(rightL)(1).x0 := 2.U
    co(rightL)(1).y0 := 1.U
    co(rightL)(1).x1 := 1.U
    co(rightL)(1).y1 := 1.U
    co(rightL)(1).x2 := 0.U
    co(rightL)(1).y2 := 1.U
    co(rightL)(1).x3 := 0.U
    co(rightL)(1).y3 := 2.U

    co(rightL)(2).x0 := 1.U
    co(rightL)(2).y0 := 2.U
    co(rightL)(2).x1 := 1.U
    co(rightL)(2).y1 := 1.U
    co(rightL)(2).x2 := 1.U
    co(rightL)(2).y2 := 0.U
    co(rightL)(2).x3 := 0.U
    co(rightL)(2).y3 := 0.U

    co(rightL)(3).x0 := 0.U
    co(rightL)(3).y0 := 1.U
    co(rightL)(3).x1 := 1.U
    co(rightL)(3).y1 := 1.U
    co(rightL)(3).x2 := 2.U
    co(rightL)(3).y2 := 1.U
    co(rightL)(3).x3 := 2.U
    co(rightL)(3).y3 := 0.U
  }
}

class CoordOffsets extends Bundle {
  val x0: UInt = UInt(2.W)
  val y0: UInt = UInt(2.W)

  val x1: UInt = UInt(2.W)
  val y1: UInt = UInt(2.W)

  val x2: UInt = UInt(2.W)
  val y2: UInt = UInt(2.W)

  val x3: UInt = UInt(2.W)
  val y3: UInt = UInt(2.W)
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