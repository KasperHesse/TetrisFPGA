
import chisel3._

class StaticBoxes extends Box {
  //Instantiate memory
  val mem = Module(new MemoryGrid)

  //Set all of our initital locations, hardcoded values
  val dd = Wire(Vec(15, UInt(10.W)))
    dd(0) := "b0000000000".U(10.W)
    dd(1) := 0.U(10.W)
    dd(2) := "b0010100000".U(10.W)
    dd(3) := "b0110110000".U(10.W)
    dd(4) := "b0100010000".U(10.W)
    dd(5) := 0.U(10.W)
    dd(6) := 0.U(10.W)
    dd(7) := 0.U(10.W)
    dd(8) := "b0000001100".U(10.W)
    dd(9) := "b0000001100".U(10.W)
    dd(10) := 0.U
    dd(11) := "b0000000001".U(10.W)
    dd(12) := 1.U(10.W)
    dd(13) := "b0001000001".U(10.W)
    dd(14) := "b0011100001".U(10.W)

  //Procedure for making default assignments
  val cntY = RegInit(0.U(4.W))
  val cntX = RegInit(0.U(4.W))

  //Write all 1's to the first col
  val memFinished = (cntY >= 15.U)
  val xTick = (cntX === 9.U)

  val xcoord = (io.col-160.U) >> 5 //division by 32
  val ycoord = io.row >> 5 //division by 32
  io.x := xcoord
  io.y := ycoord
  mem.io.wrData := DontCare

  when(!memFinished) {
    mem.io.wen := true.B
    mem.io.ren := false.B
    mem.io.wrData := dd(cntY)(cntX)
    mem.io.Y := cntY
    mem.io.X := cntX
    when(xTick) {
      cntX := 0.U
      cntY := cntY + 1.U
    } otherwise {
      cntX := cntX + 1.U
    }
    setColours(3.U, 3.U, 3.U)
  } .otherwise {
    //Output one colour in left/right col, use middle col for actual outputs
    mem.io.wen := false.B
    mem.io.ren := true.B
    mem.io.X := xcoord
    mem.io.Y := ycoord
    when(160.U <= io.col && io.col < 480.U)  {//Middle half of the screen
      when(mem.io.rdData) {
        setColours(15.U, 0.U, 0.U)
      }.otherwise {
        setColours(0.U, 7.U, 7.U)
      }
    } otherwise { //outer 1/4 on each side
      setColours(3.U, 3.U, 3.U)
    }
  }


}
object StaticBoxes extends App {
  chisel3.Driver.execute(args, () => new StaticBoxes())
}
