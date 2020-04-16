
import chisel3._
import chisel3.util._

class Playground extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val reg = Output(UInt(4.W))
    val done = Output(Bool())
  })

  val s1::s2::Nil = Enum(2)
  val state = RegInit(s1)
  io.done := false.B

  val myReg = RegInit(15.U(4.W))
  val reg2 = RegInit(15.U(4.W))
  io.reg := reg2
  switch(state) {
    is(s1) {
      myReg := myReg - 1.U
      when(myReg === 0.U) {
        state := s2
      }
    }

    is(s2) {
      enTesting(state === s2)
    }
  }

  def enTesting(en: Bool): Bool = {
    val done = RegInit(false.B)
    val running = RegInit(false.B)
    when(done) { //Only assert done for one clock cycle.
      done := false.B
    }

    when(rising(en)) {
      running := true.B
    }
    when(running) {
      reg2 := reg2 - 1.U
    }
    when(reg2 === 0.U) {
      running := false.B
      done := true.B
      reg2 := 15.U
    }

    done
  }
  def rising(v: Bool): Bool = v && !RegNext(v)
  /*
  val myReg = RegInit(15.U(4.W))
  io.reg := myReg

  def rising(v: Bool): Bool = v && !RegNext(v)
  def enTesting(en: Bool): Bool = {
    val done = RegInit(false.B)
    val running = RegInit(false.B)
    when(done) { //Only assert done for one clock cycle.
      done := false.B
    }

    when(rising(en)) {
      running := true.B
    }
    when(running) {
      myReg := myReg - 1.U
    }
    when(myReg === 0.U) {
      running := false.B
      done := true.B
      myReg := 15.U
    }

    done
  }

  io.done := enTesting(io.en)*/
}
