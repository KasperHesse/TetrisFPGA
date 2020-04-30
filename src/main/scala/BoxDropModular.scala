
import chisel3._
import chisel3.util._

/**
  * Datapath for the entire system
  *
  */
class BoxDropModular(maxDepth: Int = 14) extends Box {
  /*
  ===============MODULES AND DEFAULT ASSIGNMENTS================
   */

  //State machine
  val FSM = Module(new FSM())
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

  //Memory
  val mem = Module(new MemoryGrid())
  mem.io.wrData := false.B
  mem.io.wen := false.B
  mem.io.ren := false.B
  mem.io.X := 0.U
  mem.io.Y := 0.U

  //Pseudo-random number, used to select the next piece
  val rand = RegInit(0.U(16.W))
  rand := Mux(io.btnR || io.btnU || io.btnL || io.btnD, rand + 1.U, rand)

  /*
  ========== COORDINATES AND ROTATION STATE
   */
  //Rotation management
  val rotation: UInt = RegInit(0.U(2.W))
  val offsets: Vec[Vec[CoordOffsets]] = Wire(Vec(7, Vec(4, new CoordOffsets)))
  val leftS::rightS::bar::cube::t::leftL::rightL::Nil = Enum(7) //Pieces and their respective numbers
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

  val x: Bits = (io.col-160.U) >> 5 //X coordinate on game grid
  val y: Bits = io.row >> 5 //Y coordinate on game grid
  io.x := x
  io.y := y

  //Set coordinates based on offsets from base coordinate
  c0.x := baseX + offsets(piece)(rotation).x0
  c0.y := baseY + offsets(piece)(rotation).y0
  c1.x := baseX + offsets(piece)(rotation).x1
  c1.y := baseY + offsets(piece)(rotation).y1
  c2.x := baseX + offsets(piece)(rotation).x2
  c2.y := baseY + offsets(piece)(rotation).y2
  c3.x := baseX + offsets(piece)(rotation).x3
  c3.y := baseY + offsets(piece)(rotation).y3

  instantiateCoordMods()

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
      val saveFinished = saveToRAM(op(CoordCmds.savePiece)) //Enable high during this state
      when (removeFullRows(saveFinished)) { //Once finished saving, removeFullRows is enabled
        fin := true.B //Once finished removing, fin is asserted
      }
//      when( saveToRAM( op(CoordCmds.savePiece) )) { //Once saveToRAM returns true, prune all filled rows
//        fin := true.B
//      }
    }

    //Adding new pieces
    when(op(CoordCmds.addNew)) {
     val g: UInt = rand % 7.U //Just a helper signal to determine which piece to add
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
    * If successful, sets validDrop true
    * If unsuccesfull, sets validDrop false, causing 'saveToRam' to be called next
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
    * Move a piece left, except when this would cause it to overlap with another piece or go past the boundary
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
    * Move a piece right, except if this would cause it to overlap with another piece or go past the boundary
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
    * Do a flip! Flips the piece clockwise (I think)
    */
  def doFlip(): Unit = {
    //Do a flip!
    rotation := rotation + 1.U
    fin := true.B
    //Coordinates are defined from a base coordinate and other coords are offsets from this
  }

  /**
    * Saves a piece to RAM once it hits another piece or the bottom of the screen
    */
  def saveToRAM(en: Bool): Bool = {
    val saveCnt = RegInit(7.U(3.W))
    val done = WireDefault(false.B)
    //When entering, set it to 0
    when(rising(en)) {
      saveCnt := 0.U
    }
    //Loop through the pieces
    when(saveCnt < 4.U) {
      write(coords(saveCnt).x, coords(saveCnt).y, true.B)
      saveCnt := saveCnt + 1.U
    }

    when(saveCnt === 4.U) { //Once we hit the end, set cnt back to 7, mark that we're finished
//      fin := true.B
      done := true.B
      saveCnt := 7.U
    }
    done
  }

  def removeFullRows(en: Bool): Bool = {
    //Instantiating values used later on
    val xCoord = RegInit(9.U(4.W)) //X-coordinate we are reading from
    val yCoord = RegInit(14.U(4.W)) //Y-coordinate we're reading from
    val rowCnt = RegInit(0.U(3.W)) //How many rows should be removed
    val rowsToRemove = Reg(Vec(4, UInt(4.W))) //The y-coordinates of the rows to remove
    val rdData = WireDefault(false.B) //The currently read value from memory
    val rdReg = RegNext(rdData) //The value read on the previous clock cycle
    val nextLine = !rdData || (xCoord === 0.U) //Used when state=checking, to indicate that a lines has been processed

    //These two values are used in reading/writing sections
    val rw_x = xCoord
    val rw_y = rowsToRemove(rowCnt - 1.U) //Using (-1) to get correct value.
    //If eq rowsToRemove=1, we need to select the element at index 0

    //Default assignments
    val done = WireInit(false.B)
//    io.mem.ren := false.B
//    io.mem.wen := false.B
//    io.mem.X := xCoord
//    io.mem.Y := yCoord
//    io.mem.wrData := false.B


    //State register
    val idle :: checking :: reading :: writing :: finished :: Nil = Enum(5)
    val state = RegInit(idle);

    switch(state) {
      is(idle) {
        when(en) { //Leave idle state once enable goes high
          state := checking
        }
      }

      //State for checking the values stored at all 150 grid locations
      is(checking) {
        rdData := read(xCoord, yCoord) //Read a value from memory
        when(rdData) { //If set, keep processing current line
          when(xCoord > 0.U) {
            xCoord := xCoord - 1.U
          }.otherwise { //10 set values read in a row, the current line is full
            rowsToRemove(rowCnt) := yCoord
            rowCnt := rowCnt + 1.U
          }
        }

        when(nextLine) { //Row finished or false value read. Move up
          xCoord := 9.U
          yCoord := yCoord - 1.U
        }
        when(nextLine && (yCoord === 0.U)) { //We're finished
          state := reading
          xCoord := 9.U
          yCoord := 14.U
        }
      }

      //State for reading a value at (x,y-1) and subsequently writing it to (x,y)
      //effectively moving all lines down by one
      is(reading) { //When entering this, xCoord should already be equal to 9
        when(rowCnt === 0.U) { //No more rows, or none to begin with
          state := finished
        }.otherwise {
          state := writing
          rdData := read(rw_x, rw_y - 1.U) //Reading at y-1 to read the row above
        }
      }

      //State for writing the value obtained in state 'reading'
      is(writing) {
        write(rw_x, rw_y, rdReg) //We need to write using the value obtained in previous clock cycle
        when(xCoord > 0.U) { //Still processing a row
          xCoord := xCoord - 1.U
        }.otherwise { //xcoord = 0, row finished
          xCoord := 9.U
          rw_y := rw_y - 1.U
        }

        when(rw_x === 0.U && rw_y === 1.U) { //Finished moving all blocks
          rowCnt := rowCnt - 1.U
        }
        state := reading
      }

      //Finished processing
      is(finished) {
        done := true.B //Set return variable true
        xCoord := 9.U //Reset coordinate vectors
        yCoord := 14.U //Reset
        when(!en) { //Once enable is deasserted, return to idle state
          state := idle
        }
      }
    }
    return done
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
    * Draws all boxes on the game screen.
    * The currently falling piece is drawn green
    * Previous pieces are red
    * The game background is teal
    * Areas not part of the game are grey
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

  /**
    * Increase the x coordinate of the current block, moving it right
    * @param v The amount to increase by
    */
  def addX(v: Int): Unit = {
    baseX := baseX + v.U
  }

  /**
    * Decrease the x-coordinate of the current block, moving it left
    * @param v The amount to decrease by
    */
  def subX(v: Int): Unit = {
    baseX := baseX - v.U
  }

  /**
    * Increase the y-coordinate of the current block, moving it down
    * @param v The amount to increase by
    */
  def addY(v: Int): Unit = {
    baseY := baseY + v.U
  }
/*
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
 */

  def instantiateCoordMods(): Unit = {

    /*
    Coordinate modifiers for left squiggly
     */
    offsets(leftS)(0).x0 := 1.U
    offsets(leftS)(0).y0 := 0.U
    offsets(leftS)(0).x1 := 1.U
    offsets(leftS)(0).y1 := 1.U
    offsets(leftS)(0).x2 := 2.U
    offsets(leftS)(0).y2 := 1.U
    offsets(leftS)(0).x3 := 2.U
    offsets(leftS)(0).y3 := 2.U

    offsets(leftS)(1).x0 := 2.U
    offsets(leftS)(1).y0 := 1.U
    offsets(leftS)(1).x1 := 1.U
    offsets(leftS)(1).y1 := 1.U
    offsets(leftS)(1).x2 := 1.U
    offsets(leftS)(1).y2 := 2.U
    offsets(leftS)(1).x3 := 0.U
    offsets(leftS)(1).y3 := 2.U

    offsets(leftS)(2).x0 := 1.U
    offsets(leftS)(2).y0 := 0.U
    offsets(leftS)(2).x1 := 1.U
    offsets(leftS)(2).y1 := 1.U
    offsets(leftS)(2).x2 := 2.U
    offsets(leftS)(2).y2 := 1.U
    offsets(leftS)(2).x3 := 2.U
    offsets(leftS)(2).y3 := 2.U

    offsets(leftS)(3).x0 := 2.U
    offsets(leftS)(3).y0 := 1.U
    offsets(leftS)(3).x1 := 1.U
    offsets(leftS)(3).y1 := 1.U
    offsets(leftS)(3).x2 := 1.U
    offsets(leftS)(3).y2 := 2.U
    offsets(leftS)(3).x3 := 0.U
    offsets(leftS)(3).y3 := 2.U

    /*
    Coordinate modifiers for right squiggly
     */
    offsets(rightS)(0).x0 := 2.U
    offsets(rightS)(0).y0 := 0.U
    offsets(rightS)(0).x1 := 2.U
    offsets(rightS)(0).y1 := 1.U
    offsets(rightS)(0).x2 := 1.U
    offsets(rightS)(0).y2 := 1.U
    offsets(rightS)(0).x3 := 1.U
    offsets(rightS)(0).y3 := 2.U

    offsets(rightS)(1).x0 := 2.U
    offsets(rightS)(1).y0 := 2.U
    offsets(rightS)(1).x1 := 1.U
    offsets(rightS)(1).y1 := 2.U
    offsets(rightS)(1).x2 := 1.U
    offsets(rightS)(1).y2 := 1.U
    offsets(rightS)(1).x3 := 0.U
    offsets(rightS)(1).y3 := 1.U

    offsets(rightS)(2).x0 := 2.U
    offsets(rightS)(2).y0 := 0.U
    offsets(rightS)(2).x1 := 2.U
    offsets(rightS)(2).y1 := 1.U
    offsets(rightS)(2).x2 := 1.U
    offsets(rightS)(2).y2 := 1.U
    offsets(rightS)(2).x3 := 1.U
    offsets(rightS)(2).y3 := 2.U

    offsets(rightS)(3).x0 := 2.U
    offsets(rightS)(3).y0 := 2.U
    offsets(rightS)(3).x1 := 1.U
    offsets(rightS)(3).y1 := 2.U
    offsets(rightS)(3).x2 := 1.U
    offsets(rightS)(3).y2 := 1.U
    offsets(rightS)(3).x3 := 0.U
    offsets(rightS)(3).y3 := 1.U

    /*
    Coordinate modifiers for bar
     */
    offsets(bar)(0).x0 := 2.U
    offsets(bar)(0).y0 := 0.U
    offsets(bar)(0).x1 := 2.U
    offsets(bar)(0).y1 := 1.U
    offsets(bar)(0).x2 := 2.U
    offsets(bar)(0).y2 := 2.U
    offsets(bar)(0).x3 := 2.U
    offsets(bar)(0).y3 := 3.U

    offsets(bar)(1).x0 := 0.U
    offsets(bar)(1).y0 := 2.U
    offsets(bar)(1).x1 := 1.U
    offsets(bar)(1).y1 := 2.U
    offsets(bar)(1).x2 := 2.U
    offsets(bar)(1).y2 := 2.U
    offsets(bar)(1).x3 := 3.U
    offsets(bar)(1).y3 := 2.U

    offsets(bar)(2).x0 := 2.U
    offsets(bar)(2).y0 := 0.U
    offsets(bar)(2).x1 := 2.U
    offsets(bar)(2).y1 := 1.U
    offsets(bar)(2).x2 := 2.U
    offsets(bar)(2).y2 := 2.U
    offsets(bar)(2).x3 := 2.U
    offsets(bar)(2).y3 := 3.U

    offsets(bar)(3).x0 := 0.U
    offsets(bar)(3).y0 := 2.U
    offsets(bar)(3).x1 := 1.U
    offsets(bar)(3).y1 := 2.U
    offsets(bar)(3).x2 := 2.U
    offsets(bar)(3).y2 := 2.U
    offsets(bar)(3).x3 := 3.U
    offsets(bar)(3).y3 := 2.U

    /*
    Coordinate modifiers for cube
     */
    offsets(cube)(0).x0 := 1.U
    offsets(cube)(0).y0 := 0.U
    offsets(cube)(0).x1 := 1.U
    offsets(cube)(0).y1 := 1.U
    offsets(cube)(0).x2 := 2.U
    offsets(cube)(0).y2 := 1.U
    offsets(cube)(0).x3 := 2.U
    offsets(cube)(0).y3 := 0.U

    offsets(cube)(1).x0 := 1.U
    offsets(cube)(1).y0 := 0.U
    offsets(cube)(1).x1 := 1.U
    offsets(cube)(1).y1 := 1.U
    offsets(cube)(1).x2 := 2.U
    offsets(cube)(1).y2 := 1.U
    offsets(cube)(1).x3 := 2.U
    offsets(cube)(1).y3 := 0.U

    offsets(cube)(2).x0 := 1.U
    offsets(cube)(2).y0 := 0.U
    offsets(cube)(2).x1 := 1.U
    offsets(cube)(2).y1 := 1.U
    offsets(cube)(2).x2 := 2.U
    offsets(cube)(2).y2 := 1.U
    offsets(cube)(2).x3 := 2.U
    offsets(cube)(2).y3 := 0.U

    offsets(cube)(3).x0 := 1.U
    offsets(cube)(3).y0 := 0.U
    offsets(cube)(3).x1 := 1.U
    offsets(cube)(3).y1 := 1.U
    offsets(cube)(3).x2 := 2.U
    offsets(cube)(3).y2 := 1.U
    offsets(cube)(3).x3 := 2.U
    offsets(cube)(3).y3 := 0.U

    offsets(t)(0).x0 := 1.U
    offsets(t)(0).y0 := 2.U
    offsets(t)(0).x1 := 1.U
    offsets(t)(0).y1 := 1.U
    offsets(t)(0).x2 := 1.U
    offsets(t)(0).y2 := 0.U
    offsets(t)(0).x3 := 2.U
    offsets(t)(0).y3 := 1.U

    offsets(t)(1).x0 := 0.U
    offsets(t)(1).y0 := 1.U
    offsets(t)(1).x1 := 1.U
    offsets(t)(1).y1 := 1.U
    offsets(t)(1).x2 := 2.U
    offsets(t)(1).y2 := 1.U
    offsets(t)(1).x3 := 1.U
    offsets(t)(1).y3 := 2.U

    offsets(t)(2).x0 := 1.U
    offsets(t)(2).y0 := 2.U
    offsets(t)(2).x1 := 1.U
    offsets(t)(2).y1 := 1.U
    offsets(t)(2).x2 := 1.U
    offsets(t)(2).y2 := 0.U
    offsets(t)(2).x3 := 0.U
    offsets(t)(2).y3 := 1.U

    offsets(t)(3).x0 := 2.U
    offsets(t)(3).y0 := 1.U
    offsets(t)(3).x1 := 1.U
    offsets(t)(3).y1 := 1.U
    offsets(t)(3).x2 := 0.U
    offsets(t)(3).y2 := 1.U
    offsets(t)(3).x3 := 1.U
    offsets(t)(3).y3 := 0.U

    offsets(leftL)(0).x0 := 1.U
    offsets(leftL)(0).y0 := 0.U
    offsets(leftL)(0).x1 := 1.U
    offsets(leftL)(0).y1 := 1.U
    offsets(leftL)(0).x2 := 1.U
    offsets(leftL)(0).y2 := 2.U
    offsets(leftL)(0).x3 := 0.U
    offsets(leftL)(0).y3 := 2.U

    offsets(leftL)(1).x0 := 2.U
    offsets(leftL)(1).y0 := 1.U
    offsets(leftL)(1).x1 := 1.U
    offsets(leftL)(1).y1 := 1.U
    offsets(leftL)(1).x2 := 0.U
    offsets(leftL)(1).y2 := 1.U
    offsets(leftL)(1).x3 := 0.U
    offsets(leftL)(1).y3 := 0.U

    offsets(leftL)(2).x0 := 1.U
    offsets(leftL)(2).y0 := 2.U
    offsets(leftL)(2).x1 := 1.U
    offsets(leftL)(2).y1 := 1.U
    offsets(leftL)(2).x2 := 1.U
    offsets(leftL)(2).y2 := 0.U
    offsets(leftL)(2).x3 := 2.U
    offsets(leftL)(2).y3 := 0.U

    offsets(leftL)(3).x0 := 0.U
    offsets(leftL)(3).y0 := 1.U
    offsets(leftL)(3).x1 := 1.U
    offsets(leftL)(3).y1 := 1.U
    offsets(leftL)(3).x2 := 2.U
    offsets(leftL)(3).y2 := 1.U
    offsets(leftL)(3).x3 := 2.U
    offsets(leftL)(3).y3 := 2.U

    offsets(rightL)(0).x0 := 1.U
    offsets(rightL)(0).y0 := 0.U
    offsets(rightL)(0).x1 := 1.U
    offsets(rightL)(0).y1 := 1.U
    offsets(rightL)(0).x2 := 1.U
    offsets(rightL)(0).y2 := 2.U
    offsets(rightL)(0).x3 := 2.U
    offsets(rightL)(0).y3 := 2.U

    offsets(rightL)(1).x0 := 2.U
    offsets(rightL)(1).y0 := 1.U
    offsets(rightL)(1).x1 := 1.U
    offsets(rightL)(1).y1 := 1.U
    offsets(rightL)(1).x2 := 0.U
    offsets(rightL)(1).y2 := 1.U
    offsets(rightL)(1).x3 := 0.U
    offsets(rightL)(1).y3 := 2.U

    offsets(rightL)(2).x0 := 1.U
    offsets(rightL)(2).y0 := 2.U
    offsets(rightL)(2).x1 := 1.U
    offsets(rightL)(2).y1 := 1.U
    offsets(rightL)(2).x2 := 1.U
    offsets(rightL)(2).y2 := 0.U
    offsets(rightL)(2).x3 := 0.U
    offsets(rightL)(2).y3 := 0.U

    offsets(rightL)(3).x0 := 0.U
    offsets(rightL)(3).y0 := 1.U
    offsets(rightL)(3).x1 := 1.U
    offsets(rightL)(3).y1 := 1.U
    offsets(rightL)(3).x2 := 2.U
    offsets(rightL)(3).y2 := 1.U
    offsets(rightL)(3).x3 := 2.U
    offsets(rightL)(3).y3 := 0.U
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


object BoxDropModular extends App {
  val a = Array("--target-dir", "output")
  chisel3.Driver.execute(a, () => new BoxDropModular(maxDepth = 14))
}