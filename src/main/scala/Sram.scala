import chisel3._
import chisel3.BlackBox
import chisel3.util._

class SramIO extends Bundle {
    val clk0 = Input(Clock())
    val csb0 = Input(Bool())
    val web0 = Input(Bool())
    val wmask0 = Input(UInt(4.W))
    val addr0 = Input(UInt(8.W))
    val din0 = Input(UInt(32.W))
    val dout0 = Output(UInt(32.W))

    val clk1 = Input(Clock())
    val csb1 = Input(Bool())
    val addr1 = Input(UInt(8.W))
    val dout1 = Output(UInt(32.W))
}

class SramMacro extends BlackBox {
    val io = IO(new SramIO)

    override def desiredName: String = s"sky130_sram_1kbyte_1rw1r_32x256_8" 
}

class SramModel extends Module {
    val io = IO(new SramIO)

   val mem = SyncReadMem(256, Vec(4, UInt(8.W)))
   val writeData = VecInit(Seq.tabulate(4) { i => io.din0(8 * i + 7, 8 * i) })
   val read0 = mem.read(io.addr0, !io.csb0 && io.web0).asUInt
   val read1 = mem.read(io.addr1, !io.csb1).asUInt

   io.dout0 := read0
   io.dout1 := read1

   when(!io.csb0 && !io.web0) {
      mem.write(io.addr0, writeData, io.wmask0.asBools)
   }
}

class OpenRAMMemory(useModel: Boolean = false) extends Module {
    val io = IO(new Bundle {
        val addr = Input(UInt(8.W))
        val din   = Input(UInt(32.W))
        val we    = Input(Bool())
        val dout  = Output(UInt(32.W))
    })

   val mem = if (useModel) Module(new SramModel).io else Module(new SramMacro).io
   mem.clk0 := clock
   mem.csb0 := false.B
   mem.web0 := !io.we
   mem.wmask0 := "b1111".U
   mem.addr0 := io.addr
   mem.din0 := io.din
   io.dout := mem.dout0

   mem.clk1 := clock
   mem.csb1 := false.B
   mem.addr1 := io.addr
   // Port 1 is wired for future read-only use; this wrapper returns port 0 data.
}

class SramProgram(useModel: Boolean = false) extends Module {
    val io = IO(new Bundle {
        val done = Output(Bool())
        val pass = Output(Bool())
        val readData = Output(UInt(32.W))
    })

   val testAddr = 5.U(8.W)
   val testData = "hdeadbeef".U(32.W)

   val idle :: write :: read :: capture :: check :: done :: Nil = Enum(6)
   val state = RegInit(idle)

   val sram = Module(new OpenRAMMemory(useModel))
   sram.io.addr := testAddr
   sram.io.din := testData
   sram.io.we := state === write

   val readDataReg = RegInit(0.U(32.W))
   val passReg = RegInit(false.B)

   switch(state) {
      is(idle) {
         state := write
      }
      is(write) {
         state := read
      }
      is(read) {
         state := capture
      }
      is(capture) {
         readDataReg := sram.io.dout
         state := check
      }
      is(check) {
         passReg := readDataReg === testData
         state := done
      }
      is(done) {
         state := done
      }
   }

   io.done := state === done
   io.pass := passReg
   io.readData := readDataReg
}

object OpenRAMMemory extends App {
    emitVerilog(new OpenRAMMemory, args = Array("--target-dir", "generated"))
}

object SramProgram extends App {
    emitVerilog(new SramProgram, args = Array("--target-dir", "generated"))
}

