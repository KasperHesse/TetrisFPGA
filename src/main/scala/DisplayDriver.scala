import chisel3._
import chisel3.util._

class DisplayDriver extends Module {
  val io = IO(new Bundle {
    val col = Input(UInt(10.W))
    val row = Input(UInt(10.W))
    val red = Output(UInt(4.W))
    val green = Output(UInt(4.W))
    val blue = Output(UInt(4.W))
    val coords = Input(Vec(4, new Coord))

    val mem = Flipped(new MemIO)
  })

  /**
    * Sets the brightness of the currently output colours, all values in the interval [0;15]
    * @param r Red brightness
    * @param g Green brightness
    * @param b Blue brightness
    */
  def setColours(r: UInt, g: UInt, b: UInt): Unit = {
    io.red := r
    io.green := g
    io.blue := b
  }

  /**
    * Performs a read from the memory position corresponding to field (x,y)
    * @param x The x coordinate of the field to check
    * @param y The y coordinate of the field to check
    * @return true if the field is set, false otherwise
    */
  def read(x: UInt, y: UInt): Bool = {
    io.mem.wen := false.B
    io.mem.ren := true.B
    io.mem.X := x
    io.mem.Y := y
    io.mem.rdData
  }
  //Default memory assignments
  io.mem.X := 0.U
  io.mem.Y := 0.U
  io.mem.wen := false.B
  io.mem.ren := false.B
  io.mem.wrData := false.B

  val x: Bits = (io.col-160.U) >> 5 //X coordinate on game grid
  val y: Bits = io.row >> 5 //Y coordinate on game grid

  //Block coordinates
  val coords = io.coords

  val t = Wire(Vec(4, Bool()))
  //Logic values for checking if any block is currently being parsed
  for(i <- 0 to 3) {
    t(i) := ((x.asUInt() === coords(i).x) && (y.asUInt() === coords(i).y))
  }

  when(160.U <= io.col && io.col < 480.U)  {//Middle half of the screen
    when(t(0) || t(1) || t(2) || t(3)) { //Currently dropping block
      setColours(0.U, 10.U, 0.U)
    } .elsewhen(read(x.asUInt(), y.asUInt())) { //Block in memory
      setColours(15.U, 0.U, 0.U)
    } .otherwise { //Rest of the game area
      setColours(0.U, 7.U, 7.U)
    }
  } otherwise { //outer 1/4 on each side
    setColours(3.U, 3.U, 3.U)
  }
}
