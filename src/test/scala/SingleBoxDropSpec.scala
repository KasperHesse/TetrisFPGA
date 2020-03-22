
import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._

class SingleBoxDropTest(dut: SingleBoxDrop) extends PeekPokeTester(dut) {
  //Reset all inputs
  poke(dut.io.btnU, 0.U)
  poke(dut.io.btnL, 0.U)
  poke(dut.io.btnR, 0.U)
  poke(dut.io.btnD, 0.U)
  poke(dut.io.row, 0.U)
  poke(dut.io.col, 0.U)
  poke(dut.io.frame, 0.U)
  poke(dut.io.vblank, 0.U)
  step(1)

  //64 frames are needed to get into updating mode
  poke(dut.io.frame, 1.U)
  poke(dut.io.vblank, true.B)

  //Count for 64 frames
  step(64)
  poke(dut.io.vblank, true.B)
  step(4)
  poke(dut.io.vblank, false.B)
  step(1)

  //Check that coord vectors are working
  val c0 = dut.io.coords(0)
  val c1 = dut.io.coords(1)
  val c2 = dut.io.coords(2)
  val c3 = dut.io.coords(3)

  expect(c0.x, 0.U)
  expect(c0.y, 0.U)

  expect(c1.x, 0.U)
  expect(c1.y, 1.U)

  expect(c2.x, 1.U)
  expect(c2.y, 1.U)

  expect(c3.x, 1.U)
  expect(c3.y, 2.U)

  //Finished checking coord vectors

  //0,0
  poke(dut.io.col, 160.U)
  poke(dut.io.row, 0.U)
  expect(dut.io.red, 0.U)
  expect(dut.io.green, 10.U)
  expect(dut.io.blue, 0.U)
  step(1)

  //0,1
  poke(dut.io.col, 160.U)
  poke(dut.io.row, 32.U)
  expect(dut.io.red, 0.U)
  expect(dut.io.green, 10.U)
  expect(dut.io.blue, 0.U)
  step(1)

  //1,1
  poke(dut.io.col, 192.U)
  poke(dut.io.row, 32.U)
  expect(dut.io.red, 0.U)
  expect(dut.io.green, 10.U)
  expect(dut.io.blue, 0.U)
  step(1)

  //2,1
  poke(dut.io.col, 192.U)
  poke(dut.io.row, 64.U)
  expect(dut.io.red, 0.U)
  expect(dut.io.green, 10.U)
  expect(dut.io.blue, 0.U)
  step(1)

  //0,2
  poke(dut.io.col, 160.U)
  poke(dut.io.row, 64.U)
  expect(dut.io.red, 0.U)
  expect(dut.io.green, 7.U)
  expect(dut.io.blue, 7.U)
  step(1)

  //0,14
  poke(dut.io.col, 160.U)
  poke(dut.io.row, 470.U)
  expect(dut.io.red, 0.U)
  expect(dut.io.green, 7.U)
  expect(dut.io.blue, 7.U)

  //vblank and frame for some time
  poke(dut.io.frame, true.B)
  poke(dut.io.vblank, true.B)

  step(121)
  expect(c0.x, 0.U)
  expect(c0.y, 1.U)

  expect(c1.x, 0.U)
  expect(c1.y, 2.U)

  expect(c2.x, 1.U)
  expect(c2.y, 2.U)

  expect(c3.x, 1.U)
  expect(c3.y, 3.U)
  poke(dut.io.vblank, false.B)
  //0,2
  poke(dut.io.col, 160.U)
  poke(dut.io.row, 64.U)
  expect(dut.io.red, 0.U)
  expect(dut.io.green, 10.U)
  expect(dut.io.blue, 0.U)
  step(1)


 }

class SingleBoxDropSpec extends FlatSpec with Matchers {
  "SingleBoxDropSpec " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new SingleBoxDrop())
    { c => new SingleBoxDropTest(c)} should be (true)
  }
}