
import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._

class SingleBoxDropTest(dut: SingleBoxDrop) extends PeekPokeTester(dut) {
  def xcolor(r: Int, g: Int, b: Int):Unit = {
    expect(dut.io.red, r.U)
    expect(dut.io.green, g.U)
    expect(dut.io.blue, b.U)
  }

  def pokexy(x: Int, y: Int):Unit = {
    poke(dut.io.col, (x*32+160).U)
    poke(dut.io.row, (y*32).U)
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

  //64 frames are needed to get into updating mode
  poke(dut.io.frame, true.B)
  poke(dut.io.vblank, true.B)
  step(64)

  //And one more frame to latch new values
  step(1)
  //poke false to enable outputs
  poke(dut.io.vblank, false.B)
  poke(dut.io.frame, false.B)

  //Check that coord vectors are working
  val c0 = dut.io.coords(0)
  val c1 = dut.io.coords(1)
  val c2 = dut.io.coords(2)
  val c3 = dut.io.coords(3)

  expect(c0.x, 5.U)
  expect(c0.y, 0.U)

  expect(c1.x, 5.U)
  expect(c1.y, 1.U)

  expect(c2.x, 4.U)
  expect(c2.y, 1.U)

  expect(c3.x, 4.U)
  expect(c3.y, 2.U)

  //Finished checking coord vectors

  //0,0
  pokexy(0,0)
  xcolor(0,7,7)
  step(1)

  //5,0
  pokexy(5,0)
  xcolor(0,10,0)
  step(1)

  //5,1
  pokexy(5,1)
  xcolor(0,10,0)
  step(1)

  //4,1
  pokexy(4,1)
  xcolor(0,10,0)
  step(1)

  //4,2
  pokexy(4,2)
  xcolor(0,10,0)
  step(1)

  //0,14
  pokexy(0,14)
  xcolor(0,7,7)

  //Set vblank and frame, step to allow boxes to drop down
  poke(dut.io.frame, true.B)
  poke(dut.io.vblank, true.B)

  step(64)
  poke(dut.io.frame, false.B)
  //And another 7 steps to let values settle
  step(7)

  expect(c0.x, 5.U)
  expect(c0.y, 1.U)

  expect(c1.x, 5.U)
  expect(c1.y, 2.U)

  expect(c2.x, 4.U)
  expect(c2.y, 2.U)

  expect(c3.x, 4.U)
  expect(c3.y, 3.U)
  poke(dut.io.vblank, false.B)
  poke(dut.io.frame, false.B)

  //5,0 // No longer a part of the squiggly
  pokexy(5,0)
  xcolor(0,7,7)
  step(1)

  //5,1
  pokexy(5,1)
  xcolor(0,10,0)
  step(1)

  //5,2
  pokexy(5,2)
  xcolor(0,10,0)
  step(1)

  //4,2
  pokexy(4,2)
  xcolor(0,10,0)
  step(1)

  //4,3
  pokexy(4,3)
  xcolor(0,10,0)
  step(1)

  //For vcd output
  poke(dut.io.vblank, true.B)
  poke(dut.io.frame, true.B)
  step(900)

  //Check the bottom
  poke(dut.io.vblank, false.B)
  poke(dut.io.frame, false.B)
  pokexy(4,14)
  xcolor(15,0,0) //This piece should be marked red after dropping down
 }

class SingleBoxDropSpec extends FlatSpec with Matchers {
  "SingleBoxDropSpec " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new SingleBoxDrop())
    { c => new SingleBoxDropTest(c)} should be (true)
  }
}