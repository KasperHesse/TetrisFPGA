import chisel3._
import chisel3.util._

class BoxControl2 extends BoxDisplay {

  when(io.frame && (bdy < 460.U)) {
    bdy := bdy + 1.U
  }
  when(io.btnR) {
    bdy := 0.U
  }



}
