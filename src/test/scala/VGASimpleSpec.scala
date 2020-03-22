import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._

class VGASimpleTest(dut: VGASimple) extends PeekPokeTester(dut) {
  expect(dut.io.red, 15.U)
  expect(dut.io.green, 0.U)
  expect(dut.io.blue, 0.U)
  expect(dut.io.hsync, true)
  expect(dut.io.vsync, true)
  step(639)
  expect(dut.io.red, 15.U)
  step(1)
  expect(dut.io.red, 0.U)
  step(16)
  expect(dut.io.hsync, false)
  step(96)
  expect(dut.io.hsync, true)
  step(48)
  expect(dut.io.red, 15.U)
}

class VGASimpleSpec extends FlatSpec with Matchers {
  "VGASimple " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new VGASimple())
    { c => new VGASimpleTest(c)} should be (true)
  }
}