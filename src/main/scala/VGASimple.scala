import chisel3._
import chisel3.util._

class VGASimple extends Module {
  val io = IO(new Bundle{
    val red: UInt = Output(UInt(4.W))
    val green: UInt = Output(UInt(4.W))
    val blue: UInt = Output(UInt(4.W))
    val hsync: Bool = Output(Bool())
    val vsync: Bool = Output(Bool())
  })

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

  val denable: Bool = timing.io.d_enable;


  when(denable) {
    io.red := "xF".U(4.W)
    io.green := "x0".U(4.W)
    io.blue := "x0".U(4.W)
  } otherwise {
    io.red := 0.U(4.W)
    io.green := 0.U(4.W)
    io.blue := 0.U(4.W)
  }

  io.hsync := timing.io.hsync
  io.vsync := timing.io.vsync
}

object VGASimple extends App {
  chisel3.Driver.execute(args, () => new VGASimple())
}
