
import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._

class StaticBoxesTest(dut: StaticBoxes) extends PeekPokeTester(dut) {
  //Reset all inputs
  poke(dut.io.btnU, 0.U)
  poke(dut.io.btnL, 0.U)
  poke(dut.io.btnR, 0.U)
  poke(dut.io.btnD, 0.U)
  poke(dut.io.row, 0.U)
  poke(dut.io.col, 0.U)
  poke(dut.io.frame, 0.U)

  //Expect outputs 0 before we start
//  expect(dut.io.x, 0.U)
//  expect(dut.io.y, 0.U)
  expect(dut.io.red, 3.U)
  step(150) //To allow for initializing memory

  //Check whether our mapping of col/row and x/y coords works
  poke(dut.io.col, 160.U)
  poke(dut.io.row, 64.U)
  expect(dut.io.x, 0.U)
  expect(dut.io.y, 2.U)
  expect(dut.io.red, 0.U)
  step(1)

  poke(dut.io.col, 320.U) //5
  poke(dut.io.row, 96.U) //3
  expect(dut.io.x, 5.U)
  expect(dut.io.y, 3.U)
  expect(dut.io.red, 15.U)
  step(1)

  poke(dut.io.col, 500.U)
  poke(dut.io.row, 128.U)
  expect(dut.io.red, 3.U)
/*
  //Check whether our colours match up
  //(0,0)
  poke(dut.io.col, 0.U)
  poke(dut.io.row, 0.U)
//  expect(dut.io.x, 0.U)
//  expect(dut.io.y, 0.U)
  expect(dut.io.red, 3.U)
  //(7,0)
  poke(dut.io.col, 511.U)
  expect(dut.io.red, 0.U)

  //(8,0) (lidt inde)
  poke(dut.io.col, 520.U)
  expect(dut.io.red, 15.U)

  //(4,4) /true
  poke(dut.io.col, (4*64).U)
  poke(dut.io.row, (4*32).U)
  expect(dut.io.red, 15.U)

  //(5,8), false
  poke(dut.io.col, (5*64).U)
  poke(dut.io.row, (8*32).U)
  expect(dut.io.red, 0.U)
  /*
  poke(dut.io.row, 32.U)
  expect(dut.io.red, 15.U)

  //Expect no colours here
  poke(dut.io.col, 64.U)
  expect(dut.io.red, 0.U)
 */*/
 }

class StaticBoxesSpec extends FlatSpec with Matchers {
  "BoxDisplay2 " should "pass" in {
    chisel3.iotesters.Driver.execute(Array("--generate-vcd-output", "on"), () => new StaticBoxes())
    { c => new StaticBoxesTest(c)} should be (true)
  }
}