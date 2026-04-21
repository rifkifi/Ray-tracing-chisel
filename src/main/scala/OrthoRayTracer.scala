import chisel3._
import chisel3.util._

case class SphereConfig(
  cx: Int,
  cy: Int,
  cz: Int,
  rSq: Int,
)

class OrthoSphereConfig(
  val cxBits: Int,
  val cyBits: Int,
  val czBits: Int,
  val rSqBits: Int,
) extends Bundle {
  val cx = SInt(cxBits.W)
  val cy = SInt(cyBits.W)
  val cz = SInt(czBits.W)
  val rSq = UInt(rSqBits.W)
}

class OrthoRenderConfig(
  val maxSpheres: Int,
  val cxBits: Int,
  val cyBits: Int,
  val czBits: Int,
  val rSqBits: Int,
  val lightBits: Int,
) extends Bundle {
  val numSpheres = UInt(math.max(1, log2Ceil(maxSpheres + 1)).W)
  val lightX = SInt(lightBits.W)
  val lightY = SInt(lightBits.W)
  val spheres = Vec(maxSpheres, new OrthoSphereConfig(cxBits, cyBits, czBits, rSqBits))
}

class OrthoRenderOutput extends Bundle {
  val busy = Output(Bool())
  val frameComplete = Output(Bool())
  val asciiChar = Output(UInt(8.W))
  val asciiValid = Output(Bool())
  val x = Output(UInt(log2Ceil(80).W))
  val y = Output(UInt(log2Ceil(40).W))
}

class OrthoRayTracerCore(
  val maxSpheres: Int,
  val cxBits: Int = 8,
  val cyBits: Int = 7,
  val czBits: Int = 8,
  val rSqBits: Int = 14,
  val lightBits: Int = 8,
) extends Module {
  require(maxSpheres >= 1, "At least one sphere slot is required")
  require(cxBits >= 2, "cxBits must be at least 2")
  require(cyBits >= 2, "cyBits must be at least 2")
  require(czBits >= 2, "czBits must be at least 2")
  require(rSqBits >= 2, "rSqBits must be at least 2")
  require(lightBits >= 2, "lightBits must be at least 2")

  val WIDTH = 80
  val HEIGHT = 40
  val NUM_CHARS = 10
  val COUNT_BITS = math.max(1, log2Ceil(maxSpheres + 1))
  val CHAR_BITS = log2Ceil(NUM_CHARS)
  val sphereConfigType = new OrthoSphereConfig(cxBits, cyBits, czBits, rSqBits)
  val renderConfigType =
    new OrthoRenderConfig(maxSpheres, cxBits, cyBits, czBits, rSqBits, lightBits)

  val io = IO(new Bundle {
    val start = Input(Bool())
    val configValid = Input(Bool())
    val config = Input(renderConfigType)
    val out = new OrthoRenderOutput
  })

  val ASCII_CHARS = VecInit(Seq(
    ' ', '.', ',', ':', ';', 'o', 'x', '%', '#', '@'
  ).map(_.U(8.W)))

  object State extends ChiselEnum {
    val idle, test, output = Value
  }

  val state = RegInit(State.idle)
  val pixelX = RegInit(0.U(log2Ceil(WIDTH).W))
  val pixelY = RegInit(0.U(log2Ceil(HEIGHT).W))
  val sIdx = RegInit(0.U(COUNT_BITS.W))
  val hitFound = RegInit(false.B)
  val hitChar = RegInit(0.U(CHAR_BITS.W))
  val numSpheresReg = RegInit(0.U(COUNT_BITS.W))
  val lightXReg = RegInit(0.S(lightBits.W))
  val lightYReg = RegInit(0.S(lightBits.W))
  val sphereRegs = Reg(Vec(maxSpheres, sphereConfigType))

  val px = pixelX.zext - (WIDTH / 2).S
  val py = (HEIGHT / 2).S - pixelY.zext

  val sphereActive = Wire(Vec(maxSpheres, Bool()))
  val sphereRank = Wire(Vec(maxSpheres, UInt(COUNT_BITS.W)))
  val currentSphereOH = Wire(Vec(maxSpheres, Bool()))

  for (i <- 0 until maxSpheres) {
    sphereActive(i) := i.U < numSpheresReg

    val earlierSpheres = (0 until maxSpheres).map { j =>
      val sameDepthEarlierIndex =
        (sphereRegs(j).cz === sphereRegs(i).cz) && (if (j < i) true.B else false.B)
      sphereActive(j) && ((sphereRegs(j).cz < sphereRegs(i).cz) || sameDepthEarlierIndex)
    }

    sphereRank(i) := PopCount(earlierSpheres)
    currentSphereOH(i) := sphereActive(i) && (sphereRank(i) === sIdx)
  }

  val currentSphereValid = currentSphereOH.asUInt.orR
  val currentCx = Mux(currentSphereValid, Mux1H(currentSphereOH, sphereRegs.map(_.cx)), 0.S(cxBits.W))
  val currentCy = Mux(currentSphereValid, Mux1H(currentSphereOH, sphereRegs.map(_.cy)), 0.S(cyBits.W))
  val currentRSq = Mux(currentSphereValid, Mux1H(currentSphereOH, sphereRegs.map(_.rSq)), 0.U(rSqBits.W))

  io.out.busy := state =/= State.idle
  io.out.frameComplete := false.B
  io.out.asciiValid := false.B
  io.out.asciiChar := ASCII_CHARS(0)
  io.out.x := 0.U
  io.out.y := 0.U

  switch(state) {
    is(State.idle) {
      when(io.start && io.configValid) {
        for (i <- 0 until maxSpheres) {
          sphereRegs(i) := io.config.spheres(i)
        }
        numSpheresReg := io.config.numSpheres
        lightXReg := io.config.lightX
        lightYReg := io.config.lightY
        pixelX := 0.U
        pixelY := 0.U
        sIdx := 0.U
        hitFound := false.B

        when(io.config.numSpheres === 0.U) {
          state := State.output
        }.otherwise {
          state := State.test
        }
      }
    }

    is(State.test) {
      val dx = px - currentCx
      val dy = py - currentCy
      val dSq = (dx * dx + dy * dy).asUInt
      val hit = currentSphereValid && (dSq < currentRSq)
      val shadeDx = dx - lightXReg
      val shadeDy = dy - lightYReg
      val shiftedShadeSq = (shadeDx * shadeDx + shadeDy * shadeDy).asUInt
      val blendedShadeSq = dSq + (shiftedShadeSq * 15.U)

      val cIdx = Wire(UInt(CHAR_BITS.W))
      cIdx := 0.U
      for (i <- 0 until NUM_CHARS) {
        when(blendedShadeSq <= (currentRSq * (100 - i * i).U * 16.U) / 100.U) {
          cIdx := i.U
        }
      }

      when(hit) {
        hitFound := true.B
        hitChar := cIdx
      }

      when(sIdx + 1.U >= numSpheresReg) {
        sIdx := 0.U
        state := State.output
      }.otherwise {
        sIdx := sIdx + 1.U
      }
    }

    is(State.output) {
      val bumped = hitChar +& 1.U
      val clamped = Mux(
        bumped >= NUM_CHARS.U,
        (NUM_CHARS - 1).U(CHAR_BITS.W),
        bumped(CHAR_BITS - 1, 0),
      )
      val charIdx = Mux(hitFound, clamped, 0.U)

      io.out.asciiChar := ASCII_CHARS(charIdx)
      io.out.asciiValid := true.B
      io.out.x := pixelX
      io.out.y := pixelY

      hitFound := false.B

      when(pixelX === (WIDTH - 1).U && pixelY === (HEIGHT - 1).U) {
        state := State.idle
        io.out.frameComplete := true.B
      }.elsewhen(pixelX === (WIDTH - 1).U) {
        pixelX := 0.U
        pixelY := pixelY + 1.U
        when(numSpheresReg === 0.U) {
          state := State.output
        }.otherwise {
          state := State.test
        }
      }.otherwise {
        pixelX := pixelX + 1.U
        when(numSpheresReg === 0.U) {
          state := State.output
        }.otherwise {
          state := State.test
        }
      }
    }
  }
}

class OrthoRayTracerHardcoded(
  val maxSpheres: Int,
  val cxBits: Int = 8,
  val cyBits: Int = 7,
  val czBits: Int = 8,
  val rSqBits: Int = 14,
  val lightBits: Int = 8,
) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val busy = Output(Bool())
    val frameComplete = Output(Bool())
    val asciiChar = Output(UInt(8.W))
    val asciiValid = Output(Bool())
    val x = Output(UInt(log2Ceil(80).W))
    val y = Output(UInt(log2Ceil(40).W))
  })

  val core = Module(new OrthoRayTracerCore(maxSpheres, cxBits, cyBits, czBits, rSqBits, lightBits))
  val configType = new OrthoRenderConfig(maxSpheres, cxBits, cyBits, czBits, rSqBits, lightBits)
  val config = Wire(configType)

  config.numSpheres := 1.U
  config.lightX := 4.S(lightBits.W)
  config.lightY := 0.S(lightBits.W)
  config.spheres(0).cx := 0.S(cxBits.W)
  config.spheres(0).cy := 0.S(cyBits.W)
  config.spheres(0).cz := 0.S(czBits.W)
  config.spheres(0).rSq := 196.U(rSqBits.W)
  for (i <- 1 until maxSpheres) {
    config.spheres(i).cx := 0.S(cxBits.W)
    config.spheres(i).cy := 0.S(cyBits.W)
    config.spheres(i).cz := 0.S(czBits.W)
    config.spheres(i).rSq := 0.U(rSqBits.W)
  }

  core.io.start := io.start
  core.io.configValid := true.B
  core.io.config := config

  io.busy := core.io.out.busy
  io.frameComplete := core.io.out.frameComplete
  io.asciiChar := core.io.out.asciiChar
  io.asciiValid := core.io.out.asciiValid
  io.x := core.io.out.x
  io.y := core.io.out.y
}

class OrthoRayTracerMem(
  val maxSpheres: Int,
  val cxBits: Int = 8,
  val cyBits: Int = 7,
  val czBits: Int = 8,
  val rSqBits: Int = 14,
  val lightBits: Int = 8,
  val useMemModel: Boolean = false,
) extends Module {
  require(cxBits + cyBits <= 32, "cx/cy sphere word must fit in 32 bits")
  require(czBits + rSqBits <= 32, "cz/rSq sphere word must fit in 32 bits")

  val COUNT_BITS = math.max(1, log2Ceil(maxSpheres + 1))
  val HEADER_ADDR = 0.U(8.W)

  val io = IO(new Bundle {
    val start = Input(Bool())
    val memWriteEn = Input(Bool())
    val memWriteMask = Input(UInt(4.W))
    val memWriteAddr = Input(UInt(8.W))
    val memWriteData = Input(UInt(32.W))
    val memReadAddr = Input(UInt(8.W))
    val memReadData = Output(UInt(32.W))
    val busy = Output(Bool())
    val frameComplete = Output(Bool())
    val asciiChar = Output(UInt(8.W))
    val asciiValid = Output(Bool())
    val x = Output(UInt(log2Ceil(80).W))
    val y = Output(UInt(log2Ceil(40).W))
  })

  val core = Module(new OrthoRayTracerCore(maxSpheres, cxBits, cyBits, czBits, rSqBits, lightBits))
  val configType = new OrthoRenderConfig(maxSpheres, cxBits, cyBits, czBits, rSqBits, lightBits)

  object State extends ChiselEnum {
    val idle, loadHeaderReq, loadHeaderCap, loadSphereReq, loadSphereCap, startCore = Value
  }

  val state = RegInit(State.idle)
  val loadIdx = RegInit(0.U(COUNT_BITS.W))
  val loadWordSel = RegInit(false.B)
  val memReadAddrReg = RegInit(0.U(8.W))
  val configReg = RegInit(0.U.asTypeOf(configType))

  val mem = if (useMemModel) Module(new SramModel).io else Module(new SramMacro).io
  mem.clk0 := clock
  mem.csb0 := false.B
  mem.web0 := !io.memWriteEn
  mem.wmask0 := io.memWriteMask
  mem.addr0 := Mux(io.memWriteEn, io.memWriteAddr, io.memReadAddr)
  mem.din0 := io.memWriteData

  mem.clk1 := clock
  mem.csb1 := false.B
  mem.addr1 := memReadAddrReg

  io.memReadData := mem.dout0

  for (i <- 0 until maxSpheres) {
    when(state === State.idle) {
      configReg.spheres(i).cx := 0.S
      configReg.spheres(i).cy := 0.S
      configReg.spheres(i).cz := 0.S
      configReg.spheres(i).rSq := 0.U
    }
  }

  private def readSignedField(word: UInt, lsb: Int, width: Int): SInt = {
    word(lsb + width - 1, lsb).asSInt
  }

  core.io.start := state === State.startCore
  core.io.configValid := true.B
  core.io.config := configReg

  switch(state) {
    is(State.idle) {
      when(io.start) {
        configReg.numSpheres := 0.U
        configReg.lightX := 0.S
        configReg.lightY := 0.S
        loadIdx := 0.U
        loadWordSel := false.B
        memReadAddrReg := HEADER_ADDR
        state := State.loadHeaderReq
      }
    }

    is(State.loadHeaderReq) {
      state := State.loadHeaderCap
    }

    is(State.loadHeaderCap) {
      configReg.numSpheres := mem.dout1(COUNT_BITS - 1, 0)
      configReg.lightX := readSignedField(mem.dout1, 8, lightBits)
      configReg.lightY := readSignedField(mem.dout1, 16, lightBits)
      loadIdx := 0.U
      loadWordSel := false.B
      when(mem.dout1(COUNT_BITS - 1, 0) === 0.U) {
        state := State.startCore
      }.otherwise {
        memReadAddrReg := 1.U
        state := State.loadSphereReq
      }
    }

    is(State.loadSphereReq) {
      state := State.loadSphereCap
    }

    is(State.loadSphereCap) {
      when(!loadWordSel) {
        configReg.spheres(loadIdx).cx := readSignedField(mem.dout1, 0, cxBits)
        configReg.spheres(loadIdx).cy := readSignedField(mem.dout1, cxBits, cyBits)
        loadWordSel := true.B
        memReadAddrReg := (loadIdx << 1) + 2.U
        state := State.loadSphereReq
      }.otherwise {
        configReg.spheres(loadIdx).cz := readSignedField(mem.dout1, 0, czBits)
        configReg.spheres(loadIdx).rSq := mem.dout1(czBits + rSqBits - 1, czBits)
        loadWordSel := false.B

        when(loadIdx + 1.U >= configReg.numSpheres) {
          state := State.startCore
        }.otherwise {
          loadIdx := loadIdx + 1.U
          memReadAddrReg := (loadIdx << 1) + 3.U
          state := State.loadSphereReq
        }
      }
    }

    is(State.startCore) {
      state := State.idle
    }
  }

  io.busy := (state =/= State.idle) || core.io.out.busy
  io.frameComplete := core.io.out.frameComplete
  io.asciiChar := core.io.out.asciiChar
  io.asciiValid := core.io.out.asciiValid
  io.x := core.io.out.x
  io.y := core.io.out.y
}

object EmitOrthoRayTracerHardcoded extends App {
  println("Generating SystemVerilog for OrthoRayTracerHardcoded...")
  emitVerilog(
    new OrthoRayTracerHardcoded(
      maxSpheres = 1,
      cxBits = 6,
      cyBits = 6,
      czBits = 5,
      rSqBits = 10,
      lightBits = 5,
    ),
    Array("--target-dir", "generated"),
  )
  println("Generated: generated/OrthoRayTracerHardcoded.sv")
}

object EmitOrthoRayTracerMem extends App {
  println("Generating SystemVerilog for OrthoRayTracerMem...")
  emitVerilog(
    new OrthoRayTracerMem(
      maxSpheres = 1,
      cxBits = 6,
      cyBits = 6,
      czBits = 5,
      rSqBits = 10,
      lightBits = 5,
    ),
    Array("--target-dir", "generated"),
  )
  println("Generated: generated/OrthoRayTracerMem.sv")
}
