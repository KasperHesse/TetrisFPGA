import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._

class BoxControlTest(dut: BoxControl) extends PeekPokeTester(dut) {
  //Reset all inputs
  poke(dut.io.btnU, 0)
  poke(dut.io.btnL, 0)
  poke(dut.io.btnR, 0)
  poke(dut.io.btnD, 0)
  poke(dut.io.row, 0.U)
  poke(dut.io.col, 0.U)
  poke(dut.io.frame, 0.U)

  //Assume positions=0
  expect(dut.io.x, 0.U)
  expect(dut.io.y, 0.U)

  //Move to the right for 2 cycles
  poke(dut.io.btnR, 1)
  poke(dut.io.frame, 1)
  step(2)
  expect(dut.io.x, 2)

  //Move right and down for two cycles
  poke(dut.io.btnD, 1)
  step(2)
  expect(dut.io.x, 4)
  expect(dut.io.y, 2)

  //Deassert frame, stay still
  poke(dut.io.frame, 0)
  step(10)
  expect(dut.io.x, 4)
  expect(dut.io.y, 2)

  //Deassert movements
  poke(dut.io.btnD, 0)
  poke(dut.io.btnR, 0)

  //Check for box location. Should be at x=[4:19] and at y=[2:17]

  //Check at (0,0)
  expect(dut.io.red, 0.U)
  expect(dut.io.green, 7.U)
  expect(dut.io.blue, 7.U)

  //Check at (4,2)
  poke(dut.io.col, 4.U)
  poke(dut.io.row, 2.U)
  expect(dut.io.red, 15.U)
  expect(dut.io.green, 0.U)
  expect(dut.io.blue, 0.U)

  //Check at (3,2)
  print("Checking at (3,2)\n")
  poke(dut.io.col, 3.U)
  poke(dut.io.row, 2.U)
  expect(dut.io.red, 0.U)
  expect(dut.io.green, 7.U)
  expect(dut.io.blue, 7.U)

  //Check at (19,17), bottomright corner
  poke(dut.io.col, 19.U)
  poke(dut.io.row, 17.U)
  expect(dut.io.red, 15.U)
  expect(dut.io.green, 0.U)
  expect(dut.io.blue, 0.U)

  //Check at (20,17)
  poke(dut.io.col, 20.U)
  poke(dut.io.row, 17.U)
  expect(dut.io.red, 0.U)
  expect(dut.io.green, 7.U)
  expect(dut.io.blue, 7.U)

  //Check at (19,18)
  poke(dut.io.col, 19.U)
  poke(dut.io.row, 18.U)
  expect(dut.io.red, 0.U)
  expect(dut.io.green, 7.U)
  expect(dut.io.blue, 7.U)
}

class BoxControlSpec extends FlatSpec with Matchers {
  "BoxControl " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new BoxControl())
    { c => new BoxControlTest(c)} should be (true)
  }
}