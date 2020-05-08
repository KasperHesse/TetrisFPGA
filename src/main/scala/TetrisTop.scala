import chisel3._
import chisel3.util._

class TetrisTop extends Module {
  val io = IO(new Bundle {
    val btnU: Bool = Input(Bool())
    val btnD: Bool = Input(Bool())
    val btnL: Bool = Input(Bool())
    val btnR: Bool = Input(Bool())
    val sw: UInt = Input(UInt(16.W))

    val red: UInt = Output(UInt(4.W))
    val green: UInt = Output(UInt(4.W))
    val blue: UInt = Output(UInt(4.W))
    val hsync: Bool = Output(Bool())
    val vsync: Bool = Output(Bool())
  })

  val red = WireInit(0.U(10.W))
  val green = WireInit(0.U(10.W))
  val blue = WireInit(0.U(10.W))

  //MODULES
  //Vga timing generator
  val timing: VGATiming = Module(new VGATiming())

  //Datapath
  val datapath = Module(new Datapath())

  //State machine
  val fsm: FSM = Module(new FSM())

  //Display driver
  val display: DisplayDriver = Module(new DisplayDriver())

  //memory
  val memGrid: MemoryGrid = Module(new MemoryGrid())

  /*
  ASSIGNMENTS
   */
  fsm.io.btnL := io.btnL
  fsm.io.btnR := io.btnR
  fsm.io.btnU := io.btnU
  fsm.io.btnD := io.btnD
  fsm.io.finished := datapath.io.fin
  fsm.io.validDrop := datapath.io.validDrop
  fsm.io.frame := timing.io.frame

  datapath.io.en := fsm.io.en
  datapath.io.op := fsm.io.op


  display.io.col := timing.io.col
  display.io.row := timing.io.row
  display.io.coords := datapath.io.coords

  val empty = Wire(Flipped(new MemIO))
  empty.rdData := 0.U
  empty.wrData := 0.U
  empty.X := 0.U
  empty.Y := 0.U
  empty.wen := 0.U
  empty.ren := 0.U

  when(timing.io.d_enable) {
    display.io.mem <> memGrid.io
    datapath.io.mem <> empty
  } .otherwise {
    datapath.io.mem <> memGrid.io
    display.io.mem <> empty
  }

  red := display.io.red
  blue := display.io.blue
  green := display.io.green

  //Make sure to output zero during blanking interval
  when(timing.io.d_enable) {
    io.red := red
    io.green := green
    io.blue := blue
  } .otherwise {
    io.red := 0.U
    io.green := 0.U
    io.blue := 0.U
  }
  //Hsync, vsync, rgb hookups
  io.hsync := timing.io.hsync
  io.vsync := timing.io.vsync
  io.red := red
  io.green := green
  io.blue := blue
}

object TetrisTop extends App {
  val a = Array("--target-dir", "output")
  chisel3.Driver.execute(a, () => new TetrisTop())
}