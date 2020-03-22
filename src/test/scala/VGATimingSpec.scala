import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._

class VGATimingTest(dut: VGATiming) extends PeekPokeTester(dut) {
  //Enter
  expect(dut.io.hsync, true)
  expect(dut.io.vsync, true)
  expect(dut.io.d_enable, true)
  expect(dut.io.vblank, false)

  //Start of H blanking
  step(10)
  expect(dut.io.hsync, true)
  expect(dut.io.vsync, true)
  expect(dut.io.d_enable, false)
  expect(dut.io.vblank, false)

  //Hsync low
  step(3)
  expect(dut.io.hsync, false)
  expect(dut.io.vsync, true)
  expect(dut.io.d_enable, false)
  expect(dut.io.vblank, false)

  //Hsync high again
  step(4)
  expect(dut.io.hsync, true)
  expect(dut.io.vsync, true)
  expect(dut.io.d_enable, false)
  expect(dut.io.vblank, false)

  //First part of new line
  step(3)
  expect(dut.io.hsync, true)
  expect(dut.io.vsync, true)
  expect(dut.io.d_enable, true)
  expect(dut.io.vblank, false)

  //Start of vblank
  step(179)
  expect(dut.io.vblank, false)
  expect(dut.io.frame, false)
  step(1)
  expect(dut.io.vblank, true)
  expect(dut.io.frame, true)
  step(1)
  expect(dut.io.frame, false)

  //move to end of frame
  step(198)
  expect(dut.io.vblank, true)
  expect(dut.io.frame, false)
  step(1)
  expect(dut.io.frame, false)
  expect(dut.io.vblank, false)
  step(200)
  expect(dut.io.vblank, true)
  expect(dut.io.frame, true)



}

class VGATimingSpec extends FlatSpec with Matchers {
  "VGATiming " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new VGATiming(
      H_DISPLAY_PERIOD = 10,
      H_FRONT_PORCH = 3,
      H_SYNC_PULSE = 4,
      H_BACK_PORCH = 3,
      V_DISPLAY_PERIOD = 10,
      V_FRONT_PORCH = 3,
      V_SYNC_PULSE = 4,
      V_BACK_PORCH = 3
    ))
    { c => new VGATimingTest(c)} should be (true)
  }
}