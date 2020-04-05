
import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest._

class BoxDropModTest(dut: BoxDropModular) extends PeekPokeTester(dut) {
  def xcolor(r: Int, g: Int, b: Int):Unit = {
    expect(dut.io.red, r.U)
    expect(dut.io.green, g.U)
    expect(dut.io.blue, b.U)
  }

  def pokexy(x: Int, y: Int):Unit = {
    poke(dut.io.col, (x*32+160).U)
    poke(dut.io.row, (y*32).U)
  }

  def xpos1(x: Int, y: Int): Unit = {
    expect(c0.x, x.U)
    expect(c0.y, y.U)
  }

  //Reset all inputs
  poke(dut.io.btnU, 0.U)
  poke(dut.io.btnL, 0.U)
  poke(dut.io.btnR, 0.U)
  poke(dut.io.btnD, 0.U)
  poke(dut.io.row, 0.U)
  poke(dut.io.col, 0.U)
  poke(dut.io.frame, 0.U)
  poke(dut.io.vblank, 0.U)

  val c0 = dut.io.coords(0)
  val c1 = dut.io.coords(1)
  val c2 = dut.io.coords(2)
  val c3 = dut.io.coords(3)

  //64 frames are needed to get into updating mode
  //150 frames to init memory
  step(1)
  step(149)


  //64 frames to get into updating mode
  poke(dut.io.frame, true.B)
  poke(dut.io.vblank, true.B)
  //Heading to sAddNew
  step(32)
  //And one more frame to latch new values
  step(1)
  //poke false to enable outputs
  poke(dut.io.vblank, false.B)
  poke(dut.io.frame, false.B)
  //Check that coord vectors are working

  expect(c0.x, 5.U)
  expect(c0.y, 0.U)

  expect(c1.x, 5.U)
  expect(c1.y, 1.U)

  expect(c2.x, 4.U)
  expect(c2.y, 1.U)

  expect(c3.x, 4.U)
  expect(c3.y, 2.U)

  //Try some different locations and colours
  pokexy(5,0)
  xcolor(0,10,0)
  pokexy(4,0)
  xcolor(0,7,7)
  pokexy(0,0)
  xcolor(0,7,7)
  poke(dut.io.row, 0.U)
  poke(dut.io.col, 0.U)
  xcolor(3,3,3)

  //Move one to the right
  poke(dut.io.frame, true.B)
  poke(dut.io.btnR, true.B)
  poke(dut.io.vblank, true.B)
  step(31)
  poke(dut.io.frame, false.B)
  //State should be moveLR, nothing should have happened yet
  xpos1(5,0)

  //1 clock cycle to enable 'running', one more to latch new values
  step(2)
  xpos1(6,0)

  //Step another 32 to hit next update cycle
  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  //Still nothing should have happpened
  xpos1(6,0)
  //Step 2 to move another field to the right
  step(2)
  xpos1(7,0)
  //And another 1 to drop
  step(1)
  xpos1(7,1)

  //Try to drop and move at frame 31
  poke(dut.io.frame, true.B)
  poke(dut.io.btnD, true.B)
  poke(dut.io.btnR, false.B)
  poke(dut.io.btnL, true.B)
  step(32)
  poke(dut.io.frame, false.B)

  //2 to move
  xpos1(7,1)
  step(2)
  xpos1(6,1)

  //1 to drop
  step(1)
  xpos1(6,2)

  //Disable left, try to drop
  poke(dut.io.frame, true.B)
  poke(dut.io.btnL, false.B)
  step(32)
  poke(dut.io.frame, false.B)
  //2 to move, 1 to drop
  step(2)
  xpos1(6,2)
  step(1)
  xpos1(6,3)


  /*

  //Should move another to the right
  step(1)
  xpos1(7,0)
   */

 }

class BoxDropModSpec extends FlatSpec with Matchers {
  "BoxDropModSpec " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new BoxDropModular())
    { c => new BoxDropModTest(c)} should be (true)
  }
}