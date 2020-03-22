import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._

class BoxControl2Test(dut: BoxControl2) extends PeekPokeTester(dut) {
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

  //First time takes 8 clock cycles
  poke(dut.io.frame, 1)
  step(128)
  expect(dut.io.x, 0)
  expect(dut.io.y, 1)
  step(128)
  expect(dut.io.y, 2)
  step(256)
  expect(dut.io.y, 4)
  poke(dut.io.btnR, 1)
  step(1)
  expect(dut.io.y, 0)
//  step(91)
//  expect(dut.io.y, 460)
//  step(1)
//  expect(dut.io.y, 460)
}

class BoxControl2Spec extends FlatSpec with Matchers {
  "BoxControl2 " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new BoxControl2())
    { c => new BoxControl2Test(c)} should be (true)
  }
}