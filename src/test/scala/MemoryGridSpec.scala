import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._

class MemoryGridTest(dut: MemoryGrid) extends PeekPokeTester(dut) {
  //Reset all inputs
  poke(dut.io.ren, false.B)
  poke(dut.io.wen, false.B)
  poke(dut.io.wrData, false.B)
  poke(dut.io.X, 0.U)
  poke(dut.io.Y, 0.U)

  //Try to read from position (0,0), expect false
  poke(dut.io.ren, true.B)
  expect(dut.io.rdData, false.B)

  //Try to write some data at (0,0), then expect true
  poke(dut.io.ren, false.B)
  poke(dut.io.wen, true.B)
  poke(dut.io.wrData, true.B)
  step(1)
  poke(dut.io.wen, false.B)
  poke(dut.io.ren, true.B)
  expect(dut.io.rdData, true.B)

  //Write a 4x1 line from (9,12) to (9,15), then try to read it
  poke(dut.io.ren, false.B)
  poke(dut.io.wen, true.B)
  poke(dut.io.wrData, true.B)
  poke(dut.io.X, 9.U)
  for(i <- 12 to 15) {
    poke(dut.io.Y, i.U)
    step(1)
  }

  poke(dut.io.ren, true.B)
  poke(dut.io.wen, false.B)
  for(i <- 12 to 15) {
    poke(dut.io.Y, i.U)
    expect(dut.io.rdData, true.B)
//    step(1)
  }

  //Unset one of the bits from previously, now expect false
  step(1)
  poke(dut.io.ren, false)
  poke(dut.io.wen, true)
  poke(dut.io.X, 9.U)
  poke(dut.io.Y, 13.U)
  poke(dut.io.wrData, false.B)
  step(1)
  poke(dut.io.wen, false)
  poke(dut.io.ren, true)
  expect(dut.io.rdData, false)
}

class MemoryGridSpec extends FlatSpec with Matchers {
  "MemoryGrid " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new MemoryGrid())
    { c => new MemoryGridTest(c)} should be (true)
  }
}