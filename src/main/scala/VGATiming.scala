import chisel3._
import chisel3.util._

class VGATiming(H_DISPLAY_PERIOD:Int=640,
                H_FRONT_PORCH:Int=16,
                H_SYNC_PULSE:Int=96,
                H_BACK_PORCH:Int=48,
                V_DISPLAY_PERIOD:Int=480,
                V_FRONT_PORCH:Int=10,
                V_SYNC_PULSE:Int=2,
                V_BACK_PORCH:Int=33) extends Module {
  val io = IO(new Bundle {
    val row = Output(UInt(10.W)) //The current row on which to draw
    val col = Output(UInt(10.W)) //The current col on which to draw
    val d_enable = Output(Bool()) //Whether the display is enabled or not (when row/col is inside visible area)
    val hsync = Output(Bool()) //Horizontal sync pulse
    val vsync = Output(Bool()) //Vertical sync pulse
    val vblank = Output(Bool()) //High during the blanking interval
    val frame = Output(Bool()) //Asserted for one pulse at the start of the blanking interval
  })

  def rising(x: Bool) = x && !RegNext(x);
 //Constants
  val H_MAX: UInt = (H_DISPLAY_PERIOD + H_FRONT_PORCH + H_SYNC_PULSE + H_BACK_PORCH).U
  val V_MAX:UInt = (V_DISPLAY_PERIOD + V_FRONT_PORCH + V_SYNC_PULSE + V_BACK_PORCH).U

  //Registers and update conditions
  val col = RegInit(0.U(10.W)) //x coordinate
  val row = RegInit(0.U(10.W)) //y coordiante
  val colUpdate:Bool = col >= (H_MAX-1.U)
  val rowUpdate:Bool = row >= V_MAX-1.U

  col := Mux(colUpdate, 0.U, col + 1.U) //Col increases each cycle, laps back to zero when hitting max

  when(rowUpdate && colUpdate) { //Start the frame over
    row := 0.U
  } .elsewhen(colUpdate && !rowUpdate) { //Line finished
    row := row + 1.U
  } //otherwise keep previous value

  //Control signals for boolean outputs
  //Vsync/hsync toggles are true during the low interval
  val HSYNC_TOGGLE: Bool = (H_DISPLAY_PERIOD + H_FRONT_PORCH).U <= col && col < (H_DISPLAY_PERIOD + H_FRONT_PORCH + H_SYNC_PULSE).U
  val VSYNC_TOGGLE: Bool = (V_DISPLAY_PERIOD + V_FRONT_PORCH).U <= row && row < (V_DISPLAY_PERIOD + V_FRONT_PORCH + V_SYNC_PULSE).U
  val VBLANK_TOGGLE: Bool = row >= (V_DISPLAY_PERIOD).U
  val DENABLE_TOGGLE: Bool = (col < H_DISPLAY_PERIOD.U) && (row < V_DISPLAY_PERIOD.U)

  io.vblank := VBLANK_TOGGLE
  io.hsync := !HSYNC_TOGGLE //Reverse polarity
  io.vsync := !VSYNC_TOGGLE //Reverse polarity
  io.d_enable := DENABLE_TOGGLE
  io.frame := rising(VBLANK_TOGGLE)

  io.row := row
  io.col := col
}
