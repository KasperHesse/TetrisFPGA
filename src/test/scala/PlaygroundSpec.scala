import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._

import scala.util.Random

class PlaygroundTest(dut: Playground) extends PeekPokeTester(dut) {
  //Reset all inputs
//  poke(dut.io.en, false.B)
//  step(1)
//  expect(dut.io.done, false.B)
//  expect(dut.io.reg, 15.U)
//  poke(dut.io.en, true.B)
//
//  for(i <- 0 to 14) {
//    step(1)
//    expect(dut.io.done, false.B)
//    expect(dut.io.reg, (15-i).U)
//  }
//  step(1)
//  expect(dut.io.done, false.B)
//  expect(dut.io.reg, 0.U)
//
//  step(1)
//  expect(dut.io.done, true.B)
//  expect(dut.io.reg, 15.U)
//
//  step(3)
//  expect(dut.io.done, false.B)
//  expect(dut.io.reg, 15.U)
//
//  poke(dut.io.en, false.B)
//  step(1)
//  expect(dut.io.done, false.B)
//  expect(dut.io.reg, 15.U)
//  poke(dut.io.en, true.B)
//
//  for(i <- 0 to 14) {
//    step(1)
//    expect(dut.io.done, false.B)
//    expect(dut.io.reg, (15-i).U)
//  }

  for(i <- 0 to 30) {
    print("Clock: ")
    print(i)
    print("Reg: ")
    print(peek(dut.io.reg))
    println()
    step(1)
  }

}

class PlaygroundSpec extends FlatSpec with Matchers {
  "Playground " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new Playground())
    { c => new PlaygroundTest(c)} should be (true)
  }
}