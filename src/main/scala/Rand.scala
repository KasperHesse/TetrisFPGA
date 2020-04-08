
import chisel3._
import chisel3.util._

/**
  * A very primite random number generator.
  * The game will always start with the same piece. By counting the number of frames where a button is pressed,
  * a pseudo-random number is generated in the subsequent turns.
  */
class Rand extends Module {
  val io = IO(new Bundle {
    val up = Input(UInt(16.W))
    val out = Output(UInt(16.W))
  })

  val cnt = RegInit(0.U(16.W))
  cnt := cnt + io.up
  io.out := cnt


}
