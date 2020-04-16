
import chisel3._
import chisel3.util._

/**
  * Datapath for the entire system
  *
  */
class BoxDropModular(maxDepth: Int) extends Box {
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
  mem.io.X := 0.S
  mem.io.Y := 0.S

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
  val baseX: SInt = RegInit(0.S(5.W))
  val baseY: SInt = RegInit(0.S(5.W))
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

  instantiateCoordMods()
  //Outer parameter: Block
  //Middle parameter: Rotation state (0=up, 1=right, 2=down, 3=left)
  //Inner parameter: Pieces

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


  /*
  ==============HELPER FUNCTIONS ARE DEFINED BELOW===============
   */
  def moveDown(): Unit = {
    //Collisions with bottom of screen
    val t = Wire(Vec(4, Bool()))
    for(i <- 0 to 3) {
      t(i) := coords(i).y === maxDepth.S
    }
    val bottomCollision = (t(0) || t(1) || t(2) || t(3))

    val rdCnt = RegInit(7.U(3.W)) //Init to 7, signals that no reads are currently happening
    //Start counting when we enter this loop
    when(rising(op(CoordCmds.down))) {
      rdCnt := 0.U
    }

    when(rdCnt < 4.U) {
      when(read(coords(rdCnt).x, coords(rdCnt).y + 1.S)) { //If we're reading something below us
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
      t(i) := coords(i).x =/= 0.S
    }

    //Check memory positions for existing pieces

    when(rising(en)) {
      movCnt := 0.U
    }
    when(movCnt < 4.U) {
      when(read( (coords(movCnt).x - 1.S), coords(movCnt).y)) { //if a piece already exists, don't try to move into place
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
      t(i) := coords(i).x =/= 9.S
    }
    //Check memory positions for existing pieces
    when(rising(en)) {
      movCnt := 0.U
    }
    when(movCnt < 4.U) {
      when(read(coords(movCnt).x + 1.S, coords(movCnt).y)) { //if a piece already exists, don't try to move into place
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
    rotation := rotation + 1.U
    fin := true.B

    //Coordinates are defined from a base coordinate and other coords are offsets from this
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
    * Checks for full lines in the tetris game, removing them and shifting down all other pieces.
    * Is run whenever a piece has been added to memory
    */
  def checkFullLines(en: Bool): Unit = {
    val row = RegInit(15.U(4.W))
    val col = RegInit(9.U(4.W))

    val rowsToRemove = Reg(Vec(4, UInt(4.W)))
    val rowRemoveCnt = RegInit(0.U(3.W))

    val idle :: check :: remove1 :: remove2 :: finished :: Nil = Enum(4)
    val state = RegInit(idle)

    /*
    When enable goes high, start checking.
    When running, start reading row/col values.
    If we reach the end of a row, add that row's number to 'rowsToRemove'

    At the end, remove any rows that need managing
     */
    switch(state) {
      is(idle) {
        rowRemoveCnt := 0.U
        when(en) {
          state := check
        }
      }

      is(check) {
        when(read(col.asSInt(), row.asSInt())) {
          col := col - 1.U
          when(col === 0.U) { //if we hit this point, it means the current row is full
            //Add the rows to our index, set back the counter
            rowsToRemove(rowRemoveCnt) := row
            rowRemoveCnt := rowRemoveCnt + 1.U
            //              col := 0.U
          }
          //If this toggles, we found a location in the row which was empty
        }.otherwise {
          col := 0.U
        }

        when(col === 0.U) { //At the end of the row
          when(row > 0.U) { //If we're not yet at the top
            row := row - 1.U
            col := 9.U
          }.otherwise { //We're at the top of the final row
            state := remove1
          }
        }
      }
      /*
      When entering this state:
        If rowRemoveCnt > 0, go through each of the rows to remove.
        Use moveLines-component and an enable-signal to start the process

      Once rowRemoveCnt hits 0, we're finished and can exit out of this process.
       */
      is(remove1) {
        val en = RegInit(false.B)

        //Go through the rows to remove
        when(rowRemoveCnt > 0.U) {
          val done = moveLines(en, rowsToRemove(rowRemoveCnt - 1.U)) //Using -1 to get correct index in array
          when(!done) { //If we're in here and not finished, enable the system
            en := true.B
          }
          when(done) { //When we're finished, disable it for a short while
            rowRemoveCnt := rowRemoveCnt - 1.U
            en := false.B
          }
        }.otherwise {
          //We're finished removing, exit out
          state := finished
        }
      }

      is(finished) {
        when(!en) {
          state := idle
        }
      }
    }
  }

  /**
    * Moves lines down in the game grid whenever a full line has been removed
    * @param en The enable signal for this method. Must be kept high for the duration of the operation
    * @param row The row which should be removed and filled with above items
    * @return
    */
  def moveLines(en: Bool, row: UInt): Bool = {
    //Logic signals used later
    val done = RegInit(false.B)
    val rowReg = RegInit(0.U(4.W))
    val colReg = RegInit(0.U(4.W))
    val readVal = RegInit(false.B)

    //States for managing where we are
    val sIdle::sRead::sWrite::sFinished::Nil = Enum(4)
    val state = RegInit(sIdle)

    switch(state) {
      is(sIdle) {
        //Latch in values on rising edge
        when(en) {
          state := sRead
          rowReg := row
          colReg := 9.U
        }
      }

      is(sRead) { //Read and keep going
        readVal := read(colReg.asSInt(), rowReg.asSInt())
        state := sWrite
      }

      is(sWrite) {
        write(colReg.asSInt(), rowReg.asSInt(), readVal)
        when(colReg > 0.U) { //Keep traversing the current row
          colReg := colReg - 1.U
        } .otherwise { //Row finished
          when(rowReg > 0.U) { //Not finished with board
            rowReg := rowReg - 1.U
            colReg := 9.U
          } .otherwise { //Finished with board
            state := sFinished
          }
        }
      }

      is(sFinished) {
        done := true.B
        when(!en) {
          state := sIdle
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
  def read(x: SInt, y: SInt): Bool = {
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
  def write(x: SInt, y: SInt, d: Bool):Unit = {
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
      t(i) := (x.asSInt() === coords(i).x && y.asSInt() === coords(i).y)
    }

    when(160.U <= io.col && io.col < 480.U)  {//Middle half of the screen
      when(t(0) || t(1) || t(2) || t(3)) { //Currently dropping block
        setColours(0.U, 10.U, 0.U)
      } .elsewhen(read(x.asSInt(), y.asSInt())) { //In memory
        setColours(15.U, 0.U, 0.U)
      } .otherwise { //Rest of the game area
        setColours(0.U, 7.U, 7.U)
      }
    } otherwise { //outer 1/4 on each side
      setColours(3.U, 3.U, 3.U)
    }
  }



  def addX(v: Int): Unit = {
//    c0.x := c0.x + v.U
//    c1.x := c1.x + v.U
//    c2.x := c2.x + v.U
//    c3.x := c3.x + v.U
    baseX := baseX + v.S
  }
  def subX(v: Int): Unit = {
//    c0.x := c0.x - v.U
//    c1.x := c1.x - v.U
//    c2.x := c2.x - v.U
//    c3.x := c3.x - v.U
    baseX := baseX - v.S
  }
  def addY(v: Int): Unit = {
//    c0.y := c0.y + v.U
//    c1.y := c1.y + v.U
//    c2.y := c2.y + v.U
//    c3.y := c3.y + v.U
    baseY := baseY + v.S
  }

  def addLeftSquiggly(): Unit = {
//    c0.x := 5.U
//    c0.y := 0.U
//
//    c1.x := 5.U
//    c1.y := 1.U
//
//    c2.x := 4.U
//    c2.y := 1.U
//
//    c3.x := 4.U
//    c3.y := 2.U
    baseX := 5.S
    baseY := 1.S
  }
  def addRightSquiggly(): Unit = {
//    c0.x := 5.U
//    c0.y := 0.U
//
//    c1.x := 5.U
//    c1.y := 1.U
//
//    c2.x := 6.U
//    c2.y := 1.U
//
//    c3.x := 6.U
//    c3.y := 2.U
    baseX := 5.S
    baseY := 1.S
  }
  def addBar(): Unit = {
//    c0.x := 5.U
//    c0.y := 0.U
//
//    c1.x := 5.U
//    c1.y := 1.U
//
//    c2.x := 5.U
//    c2.y := 2.U
//
//    c3.x := 5.U
//    c3.y := 3.U
    baseX := 5.S
    baseY := 1.S
  }
  def addCube(): Unit = {
//    c0.x := 4.U
//    c0.y := 0.U
//
//    c1.x := 5.U
//    c1.y := 0.U
//
//    c2.x := 4.U
//    c2.y := 1.U
//
//    c3.x := 5.U
//    c3.y := 1.U
    baseX := 5.S
    baseY := 0.S
  }
  def addT(): Unit = {
//    c0.x := 5.U
//    c0.y := 0.U
//
//    c1.x := 5.U
//    c1.y := 1.U
//
//    c2.x := 5.U
//    c2.y := 2.U
//
//    c3.x := 6.U
//    c3.y := 1.U
    baseX := 5.S
    baseY := 1.S
  }

  def addLeftL(): Unit = {
//    c0.x := 5.U
//    c0.y := 0.U
//
//    c1.x := 5.U
//    c1.y := 1.U
//
//    c2.x := 5.U
//    c2.y := 2.U
//
//    c3.x := 4.U
//    c3.y := 2.U
    baseX := 5.S
    baseY := 1.S
  }

  def addRightL(): Unit = {
//    c0.x := 5.U
//    c0.y := 0.U
//
//    c1.x := 5.U
//    c1.y := 1.U
//
//    c2.x := 5.U
//    c2.y := 2.U
//
//    c3.x := 6.U
//    c3.y := 2.U
    baseX := 5.S
    baseY := 1.S
  }


}

class CoordOffsets extends Bundle {
  val x0: SInt = SInt(3.W)
  val y0: SInt = SInt(3.W)

  val x1: SInt = SInt(3.W)
  val y1: SInt = SInt(3.W)

  val x2: SInt = SInt(3.W)
  val y2: SInt = SInt(3.W)

  val x3: SInt = SInt(3.W)
  val y3: SInt = SInt(3.W)
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