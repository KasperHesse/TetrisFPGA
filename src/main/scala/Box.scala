
import chisel3._
import chisel3.util._

class Box extends Module {
  val io = IO(new Bundle {
    val btnU: Bool = Input(Bool())
    val btnD: Bool = Input(Bool())
    val btnL: Bool = Input(Bool())
    val btnR: Bool = Input(Bool())
    val col: UInt = Input(UInt(10.W))
    val row: UInt = Input(UInt(10.W))
    val frame: Bool = Input(Bool())
    val vblank: Bool = Input(Bool())

    val x: UInt = Output(UInt(10.W))
    val y: UInt = Output(UInt(10.W))
    val red: UInt = Output(UInt(4.W))
    val green: UInt = Output(UInt(4.W))
    val blue: UInt = Output(UInt(4.W))

    val coords: Vec[Coord] = Output(Vec(4, new Coord))
  })
  
  val x = RegInit(0.U(4.W))

  def setColours(r: UInt, g: UInt, b: UInt): Unit = {
    io.red := r
    io.green := g
    io.blue := b
  }

  def rising(v: Bool): Bool = v && !RegNext(v)
}

//Coords are stored as signed integers to allow for easier coordinate handling when working with rotation
class Coord extends Bundle {
  val x: UInt = UInt(4.W)
  val y: UInt = UInt(4.W)
}
