import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

//=============================================================================
// Ray Tracing Types
//=============================================================================
class Ray(val bitWidth: Int = 32, val fracBits: Int = 16) extends Bundle {
  val origin = new Vector3(bitWidth, fracBits)
  val dir = new Vector3(bitWidth, fracBits)
}

class Sphere(val bitWidth: Int = 32, val fracBits: Int = 16) extends Bundle {
  val center = new Vector3(bitWidth, fracBits)
  val radius = new Fixed(bitWidth, fracBits)
  val radiusSq = new Fixed(bitWidth, fracBits)
}

class Light(val bitWidth: Int = 32, val fracBits: Int = 16) extends Bundle {
  val position = new Vector3(bitWidth, fracBits)
  val intensity = new Fixed(bitWidth, fracBits)
}

//=============================================================================
// Shared Core
//=============================================================================
class AsciiRayTracerCore(
    val maxSpheres: Int = 8
) extends Module {
  val WIDTH = 80
  val HEIGHT = 40
  val FP_WIDTH = 16
  val FP_FRAC = 8
  val INV_WIDTH = Fixed.fromDouble(0.0125, FP_WIDTH, FP_FRAC)
  val INV_HEIGHT = Fixed.fromDouble(0.025, FP_WIDTH, FP_FRAC)
  val CAMERA_POS = Fixed.fromDouble(0.0, FP_WIDTH, FP_FRAC)
  val CAMERA_POS_Z = Fixed.fromDouble(2.0, FP_WIDTH, FP_FRAC)
  val CAMERA_DIR_Z = Fixed.fromDouble(-1.0, FP_WIDTH, FP_FRAC)
  val SCREEN_WIDTH = Fixed.fromDouble(1.3963, FP_WIDTH, FP_FRAC)
  val SCREEN_HEIGHT = Fixed.fromDouble(0.6981, FP_WIDTH, FP_FRAC)
  val ZERO = Fixed.zero(FP_WIDTH, FP_FRAC)
  val ONE = Fixed.one(FP_WIDTH, FP_FRAC)
  val TWO = Fixed.fromDouble(2.0, FP_WIDTH, FP_FRAC)
  val AMBIENT = Fixed.fromDouble(0.1, FP_WIDTH, FP_FRAC)
  val EPSILON = Fixed.fromDouble(0.001, FP_WIDTH, FP_FRAC)
  val INFINITY_VAL = Fixed.fromDouble(999999.0, FP_WIDTH, FP_FRAC)
  val FORWARD = (new Vector3(FP_WIDTH, FP_FRAC)).Lit(
    _.x -> ZERO,
    _.y -> ZERO,
    _.z -> CAMERA_DIR_Z
  )
  val CAMERA_RIGHT = (new Vector3(FP_WIDTH, FP_FRAC)).Lit(
    _.x -> Fixed.fromDouble(-1.0, FP_WIDTH, FP_FRAC),
    _.y -> ZERO,
    _.z -> ZERO
  )
  val CAMERA_UP = (new Vector3(FP_WIDTH, FP_FRAC)).Lit(
    _.x -> ZERO,
    _.y -> ONE,
    _.z -> ZERO
  )
  val sphereAddrWidth = math.max(1, log2Ceil(maxSpheres))

  val io = IO(new Bundle {
    val sphereAddr = Output(UInt(sphereAddrWidth.W))
    val sphereData = Input(new Sphere(FP_WIDTH, FP_FRAC))
    val lightData = Input(new Light(FP_WIDTH, FP_FRAC))
    val start = Input(Bool())
    val busy = Output(Bool())
    val frameComplete = Output(Bool())
    val asciiChar = Output(UInt(8.W))
    val asciiValid = Output(Bool())
    val x = Output(UInt(log2Ceil(WIDTH).W))
    val y = Output(UInt(log2Ceil(HEIGHT).W))
  })

  val ASCII_CHARS = VecInit(Seq(
    ' '.U, '.'.U, ','.U, ':'.U, ';'.U, 'o'.U, 'x'.U, '%'.U, '#'.U, '@'.U
  ))

  object State extends ChiselEnum {
    val idle, generateRay, normRayDir, intersectPrep, intersectSolve, intersectEval, intersectPost, shadowSetup, shadowPrep, shadowSolve, shadowEval, shade, output = Value
  }
  val state = RegInit(State.idle)

  val pixelX = RegInit(0.U(log2Ceil(WIDTH).W))
  val pixelY = RegInit(0.U(log2Ceil(HEIGHT).W))
  val currentRay = RegInit(0.U.asTypeOf(new Ray(FP_WIDTH, FP_FRAC)))
  val shadowRay = RegInit(0.U.asTypeOf(new Ray(FP_WIDTH, FP_FRAC)))
  val currentIntensity = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val totalIntensity = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val sphereIdx = RegInit(0.U(sphereAddrWidth.W))
  val shadowSphereIdx = RegInit(0.U(sphereAddrWidth.W))
  val closestT = RegInit(INFINITY_VAL)
  val closestObj = RegInit(0.U(8.W))
  val closestCenter = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC)))
  val allTested = RegInit(false.B)
  val inShadow = RegInit(false.B)
  val hitPoint = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC)))
  val hitNormal = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC)))
  val activeLightDir = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC)))
  val activeLightDistance = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val testA = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val testB = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val testDiscriminant = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val testT = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val testHit = RegInit(false.B)
  val pipeNormVec1 = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC)))
  val pipeNormMag1 = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val pipeNormVec2 = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC)))
  val pipeNormMag2 = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))

  def normStage1(v: Vector3): Fixed = v.maxNorm()

  def normStage2(vec: Vector3, mag: Fixed): Vector3 = {
    val res = Wire(new Vector3(FP_WIDTH, FP_FRAC))
    when(mag.value > 0.U) {
      val invMag = Fixed.one(FP_WIDTH, FP_FRAC) / mag
      res := vec * invMag
    }.otherwise {
      res := vec
    }
    res
  }

  def maxFixed(a: Fixed, b: Fixed): Fixed = Mux(a > b, a, b)
  def minFixed(a: Fixed, b: Fixed): Fixed = Mux(a < b, a, b)

  io.busy := (state =/= State.idle)
  io.asciiValid := false.B
  io.frameComplete := false.B
  io.sphereAddr := Mux(
    state === State.shadowPrep || state === State.shadowSolve || state === State.shadowEval,
    shadowSphereIdx,
    sphereIdx
  )
  io.asciiChar := ASCII_CHARS(0)
  io.x := 0.U
  io.y := 0.U

  switch(state) {
    is(State.idle) {
      when(io.start) {
        pixelX := 0.U
        pixelY := 0.U
        state := State.generateRay
      }
    }

    is(State.generateRay) {
      val u = Wire(new Fixed(FP_WIDTH, FP_FRAC))
      val v = Wire(new Fixed(FP_WIDTH, FP_FRAC))
      val pixelXFixed = Wire(new Fixed(FP_WIDTH, FP_FRAC))
      val pixelYFixed = Wire(new Fixed(FP_WIDTH, FP_FRAC))

      pixelXFixed.value := pixelX << FP_FRAC
      pixelYFixed.value := pixelY << FP_FRAC
      u := pixelXFixed * INV_WIDTH
      v := pixelYFixed * INV_HEIGHT

      val uCentered = u - Fixed.half(FP_WIDTH, FP_FRAC)
      val vCentered = Fixed.half(FP_WIDTH, FP_FRAC) - v
      val rightComp = CAMERA_RIGHT * (uCentered * SCREEN_WIDTH)
      val upComp = CAMERA_UP * (vCentered * SCREEN_HEIGHT)
      val dir = FORWARD + rightComp + upComp

      currentRay.origin.x := CAMERA_POS
      currentRay.origin.y := CAMERA_POS
      currentRay.origin.z := CAMERA_POS_Z
      pipeNormVec1 := dir
      pipeNormMag1 := normStage1(dir)

      sphereIdx := 0.U
      closestT := INFINITY_VAL
      closestObj := 0.U
      closestCenter := 0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))
      allTested := false.B
      currentIntensity := ZERO

      state := State.normRayDir
    }

    is(State.normRayDir) {
      currentRay.dir := normStage2(pipeNormVec1, pipeNormMag1)
      state := State.intersectPrep
    }

    is(State.intersectPrep) {
      when(!allTested) {
        val sphere = io.sphereData
        val oc = currentRay.origin - sphere.center
        val a = currentRay.dir.dot(currentRay.dir)
        val b = (oc.dot(currentRay.dir)) * TWO
        val c = oc.dot(oc) - sphere.radiusSq

        testA := a
        testB := b
        testDiscriminant := (b * b) - ((TWO * TWO) * a * c)
        state := State.intersectSolve
      }.otherwise {
        when(closestT.value < INFINITY_VAL.value) {
          val nextHitPoint = currentRay.origin + (currentRay.dir * closestT)
          val lightVector = io.lightData.position - nextHitPoint
          hitPoint := nextHitPoint
          totalIntensity := AMBIENT
          pipeNormVec1 := nextHitPoint - closestCenter
          pipeNormMag1 := normStage1(nextHitPoint - closestCenter)
          pipeNormVec2 := lightVector
          pipeNormMag2 := normStage1(lightVector)
          activeLightDistance := lightVector.maxNorm()
          shadowSphereIdx := 0.U
          inShadow := false.B
          state := State.intersectPost
        }.otherwise {
          currentIntensity := ZERO
          state := State.output
        }
      }
    }

    is(State.intersectSolve) {
      when(testDiscriminant.value.asSInt > 0.S) {
        val sqrtDisc = testDiscriminant >> 1
        testT := (-testB - sqrtDisc) >> 1
        testHit := true.B
      }.otherwise {
        testT := ZERO
        testHit := false.B
      }
      state := State.intersectEval
    }

    is(State.intersectEval) {
      when(testHit && testT.value < closestT.value && testT.value > ZERO.value) {
        closestT := testT
        closestObj := sphereIdx
        closestCenter := io.sphereData.center
      }

      when(sphereIdx === (maxSpheres - 1).U) {
        allTested := true.B
      }.otherwise {
        sphereIdx := sphereIdx + 1.U
      }
      state := State.intersectPrep
    }

    is(State.intersectPost) {
      hitNormal := normStage2(pipeNormVec1, pipeNormMag1)
      activeLightDir := normStage2(pipeNormVec2, pipeNormMag2)
      state := State.shadowSetup
    }

    is(State.shadowSetup) {
      shadowRay.origin := hitPoint + (hitNormal * EPSILON)
      shadowRay.dir := activeLightDir
      state := State.shadowPrep
    }

    is(State.shadowPrep) {
      val sphere = io.sphereData
      val oc = shadowRay.origin - sphere.center
      val a = shadowRay.dir.dot(shadowRay.dir)
      val b = (oc.dot(shadowRay.dir)) * TWO
      val c = oc.dot(oc) - sphere.radiusSq

      testA := a
      testB := b
      testDiscriminant := (b * b) - ((TWO * TWO) * a * c)
      state := State.shadowSolve
    }

    is(State.shadowSolve) {
      when(testDiscriminant.value.asSInt > 0.S) {
        val sqrtDisc = testDiscriminant >> 1
        testT := (-testB - sqrtDisc) / (TWO * testA)
        testHit := true.B
      }.otherwise {
        testT := ZERO
        testHit := false.B
      }
      state := State.shadowEval
    }

    is(State.shadowEval) {
      val blocksLight = (shadowSphereIdx =/= closestObj) &&
        testHit && testT > EPSILON && testT < activeLightDistance

      when(blocksLight) {
        inShadow := true.B
        state := State.shade
      }.elsewhen(shadowSphereIdx === (maxSpheres - 1).U) {
        state := State.shade
      }.otherwise {
        shadowSphereIdx := shadowSphereIdx + 1.U
        state := State.shadowPrep
      }
    }

    is(State.shade) {
      val light = io.lightData

      when(!inShadow) {
        val diff = hitNormal.dot(activeLightDir)
        val diffuse = maxFixed(diff, ZERO)
        val diffuseContribution = light.intensity * diffuse
        val nextIntensity = totalIntensity + diffuseContribution

        totalIntensity := nextIntensity
        currentIntensity := minFixed(nextIntensity, ONE)
      }.otherwise {
        currentIntensity := minFixed(totalIntensity, ONE)
      }

      state := State.output
    }

    is(State.output) {
      val numChars = ASCII_CHARS.length.U
      val intensityVal = minFixed(currentIntensity, ONE).value
      val charIndex = (intensityVal * (numChars - 1.U)) >> FP_FRAC
      val safeCharIndex = Mux(charIndex >= numChars, numChars - 1.U, charIndex)(log2Ceil(ASCII_CHARS.length) - 1, 0)
      io.asciiChar := ASCII_CHARS(safeCharIndex)
      io.asciiValid := true.B
      io.x := pixelX
      io.y := pixelY

      when(pixelX === (WIDTH - 1).U && pixelY === (HEIGHT - 1).U) {
        state := State.idle
        io.frameComplete := true.B
      }.elsewhen(pixelX === (WIDTH - 1).U) {
        pixelX := 0.U
        pixelY := pixelY + 1.U
        state := State.generateRay
      }.otherwise {
        pixelX := pixelX + 1.U
        state := State.generateRay
      }
    }
  }
}

//=============================================================================
// Hardcoded Wrapper
//=============================================================================
class AsciiRayTracerHardcoded(
    val maxSpheres: Int = 8
) extends Module {
  val WIDTH = 80
  val HEIGHT = 40
  val FP_WIDTH = 16
  val FP_FRAC = 8
  val ZERO = Fixed.zero(FP_WIDTH, FP_FRAC)

  val io = IO(new Bundle {
    val start = Input(Bool())
    val busy = Output(Bool())
    val frameComplete = Output(Bool())
    val asciiChar = Output(UInt(8.W))
    val asciiValid = Output(Bool())
    val x = Output(UInt(log2Ceil(WIDTH).W))
    val y = Output(UInt(log2Ceil(HEIGHT).W))
  })

  def fixedLit(value: Double): Fixed = Fixed.fromDouble(value, FP_WIDTH, FP_FRAC)
  def makeSphere(x: Double, y: Double, z: Double, radius: Double): Sphere =
    (new Sphere(FP_WIDTH, FP_FRAC)).Lit(
      _.center.x -> fixedLit(x),
      _.center.y -> fixedLit(y),
      _.center.z -> fixedLit(z),
      _.radius -> fixedLit(radius),
      _.radiusSq -> fixedLit(radius * radius),
    )

  val defaultSphere = makeSphere(0.0, -0.2, -1.9, 0.7)
  val sphereOne = makeSphere(1.8, -0.35, -2.9, 0.5)
  val sphereTwo = makeSphere(-1.8, 0.15, -2.6, 0.5)
  val emptySphere = (new Sphere(FP_WIDTH, FP_FRAC)).Lit(
    _.center.x -> ZERO,
    _.center.y -> ZERO,
    _.center.z -> ZERO,
    _.radius -> ZERO,
    _.radiusSq -> ZERO,
  )
  val hardcodedSpheres = Wire(Vec(maxSpheres, new Sphere(FP_WIDTH, FP_FRAC)))
  hardcodedSpheres(0) := defaultSphere
  if (maxSpheres > 1) hardcodedSpheres(1) := sphereOne
  if (maxSpheres > 2) hardcodedSpheres(2) := sphereTwo
  for (i <- 3 until maxSpheres) {
    hardcodedSpheres(i) := emptySphere
  }

  val hardcodedLight = (new Light(FP_WIDTH, FP_FRAC)).Lit(
    _.position.x -> fixedLit(0.0),
    _.position.y -> fixedLit(5.0),
    _.position.z -> fixedLit(5.0),
    _.intensity -> fixedLit(0.5),
  )

  val core = Module(new AsciiRayTracerCore(maxSpheres))
  core.io.start := io.start
  core.io.sphereData := hardcodedSpheres(core.io.sphereAddr)
  core.io.lightData := hardcodedLight

  io.busy := core.io.busy
  io.frameComplete := core.io.frameComplete
  io.asciiChar := core.io.asciiChar
  io.asciiValid := core.io.asciiValid
  io.x := core.io.x
  io.y := core.io.y
}

//=============================================================================
// Macro Wrapper
//=============================================================================
class AsciiRayTracerMem(
    val maxSpheres: Int = 8,
    val useMemModel: Boolean = false
) extends Module {
  val WIDTH = 80
  val HEIGHT = 40
  val FP_WIDTH = 16
  val FP_FRAC = 8
  val sphereAddrWidth = math.max(1, log2Ceil(maxSpheres))
  val lightBaseAddr = (maxSpheres * 3).U(8.W)

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
    val x = Output(UInt(log2Ceil(WIDTH).W))
    val y = Output(UInt(log2Ceil(HEIGHT).W))
  })

  object State extends ChiselEnum {
    val idle, loadSphereReq, loadSphereCap, loadLightReq, loadLightCap, startCore = Value
  }

  val state = RegInit(State.idle)
  val loadIdx = RegInit(0.U(sphereAddrWidth.W))
  val loadWordSel = RegInit(0.U(2.W))
  val memReadAddrReg = RegInit(0.U(8.W))
  val sphereRegs = RegInit(VecInit(Seq.fill(maxSpheres)(0.U.asTypeOf(new Sphere(FP_WIDTH, FP_FRAC)))))
  val lightReg = RegInit(0.U.asTypeOf(new Light(FP_WIDTH, FP_FRAC)))

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

  val core = Module(new AsciiRayTracerCore(maxSpheres))
  core.io.start := state === State.startCore
  core.io.sphereData := sphereRegs(core.io.sphereAddr)
  core.io.lightData := lightReg

  switch(state) {
    is(State.idle) {
      when(io.start) {
        loadIdx := 0.U
        loadWordSel := 0.U
        memReadAddrReg := 0.U
        state := State.loadSphereReq
      }
    }

    is(State.loadSphereReq) {
      state := State.loadSphereCap
    }

    is(State.loadSphereCap) {
      when(loadWordSel === 0.U) {
        sphereRegs(loadIdx).center.x.value := mem.dout1(15, 0)
        sphereRegs(loadIdx).center.y.value := mem.dout1(31, 16)
        loadWordSel := 1.U
        memReadAddrReg := loadIdx * 3.U + 1.U
        state := State.loadSphereReq
      }.elsewhen(loadWordSel === 1.U) {
        sphereRegs(loadIdx).center.z.value := mem.dout1(15, 0)
        sphereRegs(loadIdx).radius.value := mem.dout1(31, 16)
        loadWordSel := 2.U
        memReadAddrReg := loadIdx * 3.U + 2.U
        state := State.loadSphereReq
      }.otherwise {
        sphereRegs(loadIdx).radiusSq.value := mem.dout1(15, 0)
        loadWordSel := 0.U
        when(loadIdx === (maxSpheres - 1).U) {
          memReadAddrReg := lightBaseAddr
          state := State.loadLightReq
        }.otherwise {
          loadIdx := loadIdx + 1.U
          memReadAddrReg := (loadIdx + 1.U) * 3.U
          state := State.loadSphereReq
        }
      }
    }

    is(State.loadLightReq) {
      state := State.loadLightCap
    }

    is(State.loadLightCap) {
      when(loadWordSel === 0.U) {
        lightReg.position.x.value := mem.dout1(15, 0)
        lightReg.position.y.value := mem.dout1(31, 16)
        loadWordSel := 1.U
        memReadAddrReg := lightBaseAddr + 1.U
        state := State.loadLightReq
      }.otherwise {
        lightReg.position.z.value := mem.dout1(15, 0)
        lightReg.intensity.value := mem.dout1(31, 16)
        loadWordSel := 0.U
        state := State.startCore
      }
    }

    is(State.startCore) {
      state := State.idle
    }
  }

  io.busy := (state =/= State.idle) || core.io.busy
  io.frameComplete := core.io.frameComplete
  io.asciiChar := core.io.asciiChar
  io.asciiValid := core.io.asciiValid
  io.x := core.io.x
  io.y := core.io.y
}

//=============================================================================
// Backward-Compatible Name
//=============================================================================
class AsciiRayTracer(
    maxSpheres: Int = 8
) extends AsciiRayTracerHardcoded(maxSpheres)

//=============================================================================
// Verilog Generators
//=============================================================================
object EmitAsciiRayTracerHardcoded extends App {
  println("Generating SystemVerilog for AsciiRayTracerHardcoded...")
  emitVerilog(
    new AsciiRayTracerHardcoded(1),
    Array("--target-dir", "generated")
  )
  println("Generated: generated/AsciiRayTracerHardcoded.sv")
}

object EmitAsciiRayTracerCore extends App{
  println("Generating SystemVerilog for AsciiRayTracerCore...")
  emitVerilog(
    new AsciiRayTracerCore(1),
    Array("--target-dir", "generated")
  )
  println("Generated: generated/AsciiRayTracerCore.sv")
}

object EmitAsciiRayTracerMem extends App {
  println("Generating SystemVerilog for AsciiRayTracerMem...")
  emitVerilog(
    new AsciiRayTracerMem(3),
    Array("--target-dir", "generated")
  )
  println("Generated: generated/AsciiRayTracerMem.sv")
}
