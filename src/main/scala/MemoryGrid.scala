
import chisel3._

class MemoryGrid(testing: Bool = false.B) extends Module {
  val io = IO( new MemIO)

  //Instantiate the memory bank
  val mem = Mem(150, Bool())

  when(io.wen) {mem.write((io.Y*10.U + io.X), io.wrData)}

  //Read data, set false as default value if we try to read without setting rdData
  //Reads the x'th position of the vector at index y
  io.rdData := false.B
  when(io.ren) {io.rdData := mem.read((io.Y*10.U + io.X))}

  //Instantiate it to false values
  val init: Bool = RegInit(false.B)
  val cnt: UInt = RegInit(0.U(8.W))
  when(!init) {
    when(!testing) {
      mem.write(cnt, false.B)
      cnt := cnt + 1.U
    } .otherwise {
      mem.write(cnt, cnt > 125.U)
      cnt := cnt + 1.U
    }
  }
  when(cnt >= 150.U) {
    init := true.B
  }
}

class MemIO extends Bundle {
  val X = Input(UInt(4.W))
  val Y = Input(UInt(4.W))
  val wrData = Input(Bool())
  val wen = Input(Bool())
  val ren = Input(Bool())
  val rdData = Output(Bool())
}

object MemoryGrid extends App {
  chisel3.Driver.execute(args, () => new MemoryGrid())
}