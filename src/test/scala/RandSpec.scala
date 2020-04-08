import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._

import scala.util.Random

class RandTest(dut: Rand) extends PeekPokeTester(dut) {
  //Reset all inputs
 step(1)
  //Read some values
  poke(dut.io.up, 1.U)
  print("  out=")
  print(peek(dut.io.out))
  println()
  step(1)

 print("  out=")
 print(peek(dut.io.out))
 println()
 step(1)

 print("  out=")
 print(peek(dut.io.out))
 println()
 step(1)

 print("  out=")
 print(peek(dut.io.out))
 println()
 step(1)
}

class RandSpec extends FlatSpec with Matchers {
  "LFSR " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new Rand())
    { c => new RandTest(c)} should be (true)
  }
}