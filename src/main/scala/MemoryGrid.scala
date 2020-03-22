
import chisel3._

class MemoryGrid extends Module {
  val io = IO(new Bundle {
    val X = Input(UInt(4.W))
    val Y = Input(UInt(4.W))
    val wrData = Input(Bool())
    val wen = Input(Bool())
    val ren = Input(Bool())
    val rdData = Output(Bool())
  })

  //Instantiate the memory bank
  //Consists of 15 vectors with 10 booleans
  //Vector number: Row. Vector position: Col
//  val mem: Mem[Vec[Bool]] = Mem(15, Vec(10, Bool()))
  val mem = Mem(150, Bool())

  when(io.wen) {mem.write(io.Y*10.U + io.X, io.wrData)}

  //Read data, set false as default value if we try to read without setting rdData
  //Reads the x'th position of the vector at index y
  io.rdData := false.B
  when(io.ren) {io.rdData := mem.read(io.Y*10.U + io.X)}
}

object MemoryGrid extends App {
  chisel3.Driver.execute(args, () => new MemoryGrid())
}