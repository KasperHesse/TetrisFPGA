
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

  def xpos0(x: Int, y: Int): Unit = {
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
  step(150)


  //32 frames to get into updating mode
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
  expect(c0.y, 1.U)

  expect(c1.x, 5.U)
  expect(c1.y, 0.U)

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
  xpos0(5,1)

  //Step over state
  step(15)
  xpos0(6,1)

  //Step another 32 to hit next update cycle
  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  //Still nothing should have happpened
  xpos0(6,1)
  //Step to move another field to the right and down
  step(15)
  xpos0(7,2)

  //Try to drop and move at frame 31
  poke(dut.io.frame, true.B)
  poke(dut.io.btnD, true.B)
  poke(dut.io.btnR, false.B)
  poke(dut.io.btnL, true.B)
  step(32)
  poke(dut.io.frame, false.B)

  //2 to move
  xpos0(7,2)
  step(15)
  xpos0(6,3)

  //Disable left, try to drop
  poke(dut.io.frame, true.B)
  poke(dut.io.btnL, false.B)
  poke(dut.io.btnD, false.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(20)
  xpos0(6,4)

  poke(dut.io.btnD, true.B)
  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  xpos0(6,5)

  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  xpos0(6,6)

  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  xpos0(6,7)

  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  xpos0(5,1)

  //Expect the previous place to be red
  poke(dut.io.vblank, false.B)
  pokexy(6,6)
  xcolor(15, 0, 0)
 }

/*
This test is broken after adding random pieces. Need to recalculate the pieces that will appear
 */
class BDMTest2(dut: BoxDropModular) extends PeekPokeTester(dut) {
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

  //Poke down to move faster
  poke(dut.io.btnD, true.B)
  poke(dut.io.vblank, true.B)
  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  //new piece should have been added
  xpos1(5,0)

  //Move down in increments of two
  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  xpos1(5,2)

  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  xpos1(5,4)

  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  xpos1(5,6)

  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  xpos1(5,1)

  //Move left once
  poke(dut.io.frame, true.B)
  poke(dut.io.btnL, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(10)
  xpos1(4,1)

  poke(dut.io.btnL, false.B)
  poke(dut.io.frame, true.B)
  //We need to move down until we're at (4,5)

  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  poke(dut.io.frame, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  xpos1(4,4)
}


class FlipTest(dut: BoxDropModular) extends PeekPokeTester(dut) {
  def xpos0(x: Int, y: Int): Unit = {
    expect(c0.x, x.U)
    expect(c0.y, y.U)
  }
  def xpos1(x: Int, y: Int): Unit = {
    expect(c1.x, x.U)
    expect(c1.y, y.U)
  }
  def xpos2(x: Int, y: Int): Unit = {
    expect(c2.x, x.U)
    expect(c2.y, y.U)
  }
  def xpos3(x: Int, y: Int): Unit = {
    expect(c3.x, x.U)
    expect(c3.y, y.U)
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
  step(150)

  //32 frames to get into updating mode
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

  xpos0(5,1)
  xpos1(5,0)
  xpos2(4,1)
  xpos3(4,2)


  //Attempt to rotate
  poke(dut.io.frame, true.B)
  poke(dut.io.btnU, true.B)
  step(31)
  poke(dut.io.frame, false.B)
  step(15)
  xpos0(5,1)
  xpos1(6,1)
  xpos2(5,0)
  xpos3(4,0)

  poke(dut.io.frame, true.B)
  poke(dut.io.btnU, true.B)
//  poke(dut.io.btnD, true.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  xpos0(5,2)
  xpos1(5,3)
  xpos2(6,2)
  xpos3(6,1)

  poke(dut.io.frame, true.B)
  poke(dut.io.btnU, true.B)
//  poke(dut.io.btnD, false.B)
  step(32)
  poke(dut.io.frame, false.B)
  step(15)
  xpos0(5,2)
  xpos1(4,2)
  xpos2(5,3)
  xpos3(6,3)
}


class BoxDropModSpec extends FlatSpec with Matchers {
  "BoxDropModSpec " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new BoxDropModular(maxDepth = 8))
    { c => new BoxDropModTest(c)} should be (true)
  }

  "FlipTest " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new BoxDropModular(maxDepth = 8))
    { c => new FlipTest(c)} should be (true)
  }

/*  "BDMTest2 " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new BoxDropModular(maxDepth = 8))
    { c => new BDMTest2(c)} should be (true)
  }*/


}