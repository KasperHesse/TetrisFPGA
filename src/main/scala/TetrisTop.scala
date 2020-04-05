import chisel3._
import chisel3.util._

class TetrisTop extends Module {
  val io = IO(new Bundle {
    val btnU: Bool = Input(Bool())
    val btnD: Bool = Input(Bool())
    val btnL: Bool = Input(Bool())
    val btnR: Bool = Input(Bool())
    val sw: UInt = Input(UInt(16.W))

    val red: UInt = Output(UInt(4.W))
    val green: UInt = Output(UInt(4.W))
    val blue: UInt = Output(UInt(4.W))
    val hsync: Bool = Output(Bool())
    val vsync: Bool = Output(Bool())
  })

  val red = WireInit(0.U(10.W))
  val green = WireInit(0.U(10.W))
  val blue = WireInit(0.U(10.W))

  val timing: VGATiming = Module(new VGATiming(
    H_DISPLAY_PERIOD = 640,
    H_FRONT_PORCH = 16,
    H_SYNC_PULSE = 96,
    H_BACK_PORCH = 48,
    V_DISPLAY_PERIOD = 480,
    V_FRONT_PORCH = 10,
    V_SYNC_PULSE = 2,
    V_BACK_PORCH = 33
  ))

  val box: Box = Module(new SingleBoxDrop)
  box.io.btnU := io.btnU
  box.io.btnL := io.btnL
  box.io.btnD := io.btnD
  box.io.btnR := io.btnR

  box.io.col := timing.io.col
  box.io.row := timing.io.row
  box.io.frame := timing.io.frame
  box.io.vblank := timing.io.vblank
  red := box.io.red
  green := box.io.green
  blue := box.io.blue

  //Make sure to output zero during blanking interval
  when(timing.io.d_enable) {
    io.red := red
    io.green := green
    io.blue := blue
  } .otherwise {
    io.red := 0.U
    io.green := 0.U
    io.blue := 0.U
  }
  //Hsync, vsync hookups
  io.hsync := timing.io.hsync
  io.vsync := timing.io.vsync
}

object TetrisTop extends App {
  val a = Array("--target-dir", "output")
  chisel3.Driver.execute(a, () => new TetrisTop())
}