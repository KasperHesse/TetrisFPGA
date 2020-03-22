
import chisel3._

class BoxDisplay extends Box {

  val bdx = RegInit(0.U(10.W))
  val bdy = RegInit(0.U(10.W))
  io.x := bdx
  io.y := bdy
  val sizeX = 16.U
  val sizeY = 16.U

  //Output logic
  when(bdx <= io.col && io.col < (bdx + sizeX) && bdy <= io.row && io.row < (bdy + sizeY)) {
    setColours(15.U, 0.U, 0.U)
  } .otherwise {
    setColours(0.U, 7.U, 7.U)
  }
}

object BoxDisplay extends App {
  chisel3.Driver.execute(args, () => new BoxDisplay())
}
