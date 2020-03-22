
import chisel3._
import chisel3.util._

class BoxControl extends BoxDisplay {
  //Constants
  val xMod: UInt = 1.U
  val yMod: UInt = 1.U

  //Movement control
  when(io.frame) {
    when(io.btnU) {
      bdy := bdy - yMod
    }.elsewhen(io.btnD) {
      bdy := bdy + yMod
    }

    when(io.btnR) {
      bdx := bdx + xMod
    }.elsewhen(io.btnL) {
      bdx := bdx - xMod
    }
  }
}
