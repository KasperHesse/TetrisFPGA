import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._

class PlaygroundTest(dut: Playground) extends PeekPokeTester(dut) {
  poke(dut.io.en, false.B)
  step(200)
  poke(dut.io.en, true.B)
  step(600)
  poke(dut.io.en, false.B)
  step(5)
  poke(dut.io.en, true.B)
  step(100)
//  expect(dut.io.finished, false.B)

}

class PlaygroundSpec extends FlatSpec with Matchers {
  "Playground " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--top-name", "Playground",
      "--target-dir", "test_run_dir/Playground",
    "--generate-vcd-output", "on") , () => new Playground())
    { c => new PlaygroundTest(c)} should be (true)
  }
}