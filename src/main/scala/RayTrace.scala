import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

//=============================================================================
// Ray Tracing Types
//=============================================================================
class Ray(val bitWidth: Int = 32, val fracBits: Int = 16) extends Bundle { // Ray represented by origin and direction.
  val origin = new Vector3(bitWidth, fracBits) // Ray origin in world space.
  val dir = new Vector3(bitWidth, fracBits) // Ray direction in world space.
}

class Sphere(val bitWidth: Int = 32, val fracBits: Int = 16) extends Bundle { // Sphere material and geometry data.
  val center = new Vector3(bitWidth, fracBits) // Sphere center position.
  val radius = new Fixed(bitWidth, fracBits) // Sphere radius.
  val radiusSq = new Fixed(bitWidth, fracBits) // Squared sphere radius for intersection math.
}

class Light(val bitWidth: Int = 32, val fracBits: Int = 16) extends Bundle { // Point light source definition.
  val position = new Vector3(bitWidth, fracBits) // Light position in world space.
  val intensity = new Fixed(bitWidth, fracBits) // Light intensity multiplier.
}

//=============================================================================
// ASCII Ray Tracer Main Module
//=============================================================================
class AsciiRayTracer(
    val maxSpheres: Int = 8 // Maximum number of spheres available through the sphere port.
) extends Module { // Main hardware ASCII ray tracer module.
  val WIDTH = 80 // Output frame width in characters.
  val HEIGHT = 40 // Output frame height in characters.
  val FP_WIDTH = 16 // Total width of fixed-point values.
  val FP_FRAC = 8 // Number of fractional bits in fixed-point values.
  val INV_WIDTH = Fixed.fromDouble(0.0125, FP_WIDTH, FP_FRAC) // Fixed-point reciprocal of the frame width.
  val INV_HEIGHT = Fixed.fromDouble(0.025, FP_WIDTH, FP_FRAC) // Fixed-point reciprocal of the frame height.
  val CAMERA_POS = Fixed.fromDouble(0.0, FP_WIDTH, FP_FRAC) // Fixed camera X/Y position.
  val CAMERA_POS_Z = Fixed.fromDouble(2.0, FP_WIDTH, FP_FRAC) // Fixed camera Z position.
  val CAMERA_DIR_Z = Fixed.fromDouble(-1.0, FP_WIDTH, FP_FRAC) // Fixed forward-facing camera direction.
  val SCREEN_WIDTH = Fixed.fromDouble(1.3963, FP_WIDTH, FP_FRAC)
  val SCREEN_HEIGHT = Fixed.fromDouble(0.6981, FP_WIDTH, FP_FRAC)
  val ZERO = Fixed.zero(FP_WIDTH, FP_FRAC) // Fixed-point zero constant.
  val ONE = Fixed.one(FP_WIDTH, FP_FRAC) // Fixed-point one constant.
  val TWO = Fixed.fromDouble(2.0, FP_WIDTH, FP_FRAC) // Fixed-point two constant.
  val AMBIENT = Fixed.fromDouble(0.1, FP_WIDTH, FP_FRAC) // Ambient light floor.
  val EPSILON = Fixed.fromDouble(0.001, FP_WIDTH, FP_FRAC) // Small offset to avoid self-intersection.
  val INFINITY_VAL = Fixed.fromDouble(999999.0, FP_WIDTH, FP_FRAC) // Large sentinel distance for nearest-hit search.
  val FORWARD = (new Vector3(FP_WIDTH, FP_FRAC)).Lit( // Constant forward-facing camera vector.
    _.x -> ZERO,
    _.y -> ZERO,
    _.z -> CAMERA_DIR_Z
  )
  val CAMERA_RIGHT = (new Vector3(FP_WIDTH, FP_FRAC)).Lit( // Constant camera right axis.
    _.x -> Fixed.fromDouble(-1.0, FP_WIDTH, FP_FRAC),
    _.y -> ZERO,
    _.z -> ZERO
  )
  val CAMERA_UP = (new Vector3(FP_WIDTH, FP_FRAC)).Lit( // Constant camera up axis.
    _.x -> ZERO,
    _.y -> ONE,
    _.z -> ZERO
  )
  val sphereAddrWidth = math.max(1, log2Ceil(maxSpheres)) // Address width for sphere indexing.
  val io = IO(new Bundle { // External module interface.
    val sphereAddr = Output(UInt(sphereAddrWidth.W)) // Address used to fetch the current sphere.
    val sphereData = Input(new Sphere(FP_WIDTH, FP_FRAC)) // Sphere data returned for the selected address.
    val lightData = Input(new Light(FP_WIDTH, FP_FRAC)) // Light data returned for the selected address.
    
    val start = Input(Bool()) // Starts rendering a new frame.
    val busy = Output(Bool()) // Indicates the renderer is actively processing.
    val frameComplete = Output(Bool()) // Pulses when the last pixel of a frame is emitted.
    
    val asciiChar = Output(UInt(8.W)) // ASCII code for the current output pixel.
    val asciiValid = Output(Bool()) // Valid signal for the ASCII output.
    val x = Output(UInt(log2Ceil(WIDTH).W)) // X coordinate of the current emitted pixel.
    val y = Output(UInt(log2Ceil(HEIGHT).W)) // Y coordinate of the current emitted pixel.
  })
  
  val ASCII_CHARS = VecInit(Seq( // ASCII ramp used to map intensity to characters.
    ' '.U, '.'.U, ','.U, ':'.U, ';'.U, 'o'.U, 'x'.U, '%'.U, '#'.U, '@'.U
  ))
  
  object State extends ChiselEnum { // Finite-state machine states for the renderer.
    val idle, generateRay, normRayDir, intersectPrep, intersectSolve, intersectEval, intersectPost, shadowSetup, shadowPrep, shadowSolve, shadowEval, shade, output = Value // Ordered rendering pipeline states.
  }
  val state = RegInit(State.idle) // Current FSM state.
  
  val pixelX = RegInit(0.U(log2Ceil(WIDTH).W)) // Current pixel X position.
  val pixelY = RegInit(0.U(log2Ceil(HEIGHT).W)) // Current pixel Y position.
  
  val currentRay = RegInit(0.U.asTypeOf(new Ray(FP_WIDTH, FP_FRAC))) // Primary ray for the current pixel.
  val shadowRay = RegInit(0.U.asTypeOf(new Ray(FP_WIDTH, FP_FRAC))) // Shadow ray toward the active light.
  
  val currentIntensity = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC)) // Final intensity selected for ASCII output.
  val totalIntensity = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC)) // Accumulated lighting intensity for the hit point.
  
  val sphereIdx = RegInit(0.U(sphereAddrWidth.W)) // Index of the sphere being tested for primary intersection.
  val shadowSphereIdx = RegInit(0.U(sphereAddrWidth.W)) // Index of the sphere being tested for shadow intersection.
  
  val closestT = RegInit(INFINITY_VAL) // Distance to the closest sphere hit found so far.
  val closestObj = RegInit(0.U(8.W)) // Index of the closest intersected sphere.
  val closestCenter = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))) // Center of the closest intersected sphere.
  val allTested = RegInit(false.B) // True when all spheres have been checked for the current ray.
  val inShadow = RegInit(false.B) // True when the active light is blocked.
  
  val hitPoint = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))) // Surface point hit by the primary ray.
  val hitNormal = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))) // Surface normal at the hit point.
  val activeLightDir = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))) // Direction from the hit point to the active light.
  val activeLightDistance = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC)) // Max-norm distance from the hit point to the active light.

  // Shared intersection pipeline registers for primary and shadow tests.
  val testA = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val testB = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val testDiscriminant = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val testT = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val testHit = RegInit(false.B)

  // Two normalization pipelines:
  // pair-1 is reused for primary ray dir and hit normal, pair-2 for light dir.
  val pipeNormVec1 = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC)))
  val pipeNormMag1 = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))
  val pipeNormVec2 = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC)))
  val pipeNormMag2 = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC))

  def normStage1(v: Vector3): Fixed = v.maxNorm() // Stage-1: magnitude estimate only.

  def normStage2(vec: Vector3, mag: Fixed): Vector3 = { // Stage-2: reciprocal and rescale.
    val res = Wire(new Vector3(FP_WIDTH, FP_FRAC))
    when(mag.value > 0.U) {
      val invMag = Fixed.one(FP_WIDTH, FP_FRAC) / mag
      res := vec * invMag
    }.otherwise {
      res := vec
    }
    res
  }
  
  def intersectSphere(ray: Ray, sphere: Sphere, minT: Fixed = ZERO, maxT: Fixed = INFINITY_VAL): (Bool, Fixed) = { // Computes whether a ray hits a sphere within the requested distance interval.
    val oc = ray.origin - sphere.center // Vector from sphere center to ray origin.
    val a = ray.dir.dot(ray.dir) // Quadratic coefficient a.
    val b = (oc.dot(ray.dir)) * TWO // Quadratic coefficient b.
    val c = oc.dot(oc) - sphere.radiusSq // Quadratic coefficient c.
    val discriminant = (b * b) - ((TWO * TWO) * a * c) // Discriminant of the quadratic equation.
    
    val hit = Wire(Bool()) // Indicates whether the sphere is hit.
    val t = Wire(new Fixed(FP_WIDTH, FP_FRAC)) // Distance to the chosen hit point.
    val discPositive = discriminant.value.asSInt > 0.S // True when the quadratic has real roots.
    
    when(discPositive) {
      val sqrtDisc = discriminant >> 1 // Approximate square root term used for root solving. 
      val t1 = (-b - sqrtDisc) / (TWO * a) // Near intersection candidate.
      
      when(t1 > minT && t1 < maxT) {
        hit := true.B
        t := t1
      }.otherwise {
        hit := false.B
        t := ZERO
      }
    }.otherwise {
      hit := false.B
      t := ZERO
    }
    
    (hit, t)
  }
  
  def maxFixed(a: Fixed, b: Fixed): Fixed = Mux(a > b, a, b) // Returns the larger of two fixed-point values.
  
  def minFixed(a: Fixed, b: Fixed): Fixed = Mux(a < b, a, b) // Returns the smaller of two fixed-point values.
  
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
    // Wait for a new frame request before starting any camera or pixel work.
    is(State.idle) {
      when(io.start) {
        pixelX := 0.U
        pixelY := 0.U
      state := State.generateRay
      }
    }
    
    // Convert the current pixel coordinate into a normalized viewing ray.
    is(State.generateRay) {
      val u = Wire(new Fixed(FP_WIDTH, FP_FRAC)) // Normalized horizontal pixel coordinate.
      val v = Wire(new Fixed(FP_WIDTH, FP_FRAC)) // Normalized vertical pixel coordinate.
      val pixelXFixed = Wire(new Fixed(FP_WIDTH, FP_FRAC)) // Current pixel X encoded as fixed-point.
      val pixelYFixed = Wire(new Fixed(FP_WIDTH, FP_FRAC)) // Current pixel Y encoded as fixed-point.
      
      pixelXFixed.value := pixelX << FP_FRAC
      pixelYFixed.value := pixelY << FP_FRAC
      u := pixelXFixed * INV_WIDTH
      v := pixelYFixed * INV_HEIGHT
      
      val uCentered = u - Fixed.half(FP_WIDTH, FP_FRAC) // Horizontal coordinate shifted around the screen center.
      val vCentered = Fixed.half(FP_WIDTH, FP_FRAC) - v // Vertical coordinate shifted around the screen center.
      val rightComp = CAMERA_RIGHT * (uCentered * SCREEN_WIDTH) // Horizontal ray contribution.
      val upComp = CAMERA_UP * (vCentered * SCREEN_HEIGHT) // Vertical ray contribution.
      val dir = FORWARD + rightComp + upComp // Unnormalized ray direction through the current pixel.
      
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

    // Complete primary-ray normalization in its own cycle.
    is(State.normRayDir) {
      currentRay.dir := normStage2(pipeNormVec1, pipeNormMag1)
      state := State.intersectPrep
    }
    
    // Primary-intersection stage 1: coefficients and discriminant.
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
          val nextHitPoint = currentRay.origin + (currentRay.dir * closestT) // Computed closest world-space hit position.
          val lightVector = io.lightData.position - nextHitPoint // Vector from the hit point to the light.
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

    // Primary-intersection stage 2: solve candidate t.
    is(State.intersectSolve) {
      when(testDiscriminant.value.asSInt > 0.S) {
        val sqrtDisc = testDiscriminant >> 1
        testT := (-testB - sqrtDisc) / (TWO * testA)
        testHit := true.B
      }.otherwise {
        testT := ZERO
        testHit := false.B
      }
      state := State.intersectEval
    }

    // Primary-intersection stage 3: evaluate and advance sphere loop.
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

    // Complete hit-normal and light-direction normalization in parallel.
    is(State.intersectPost) {
      hitNormal := normStage2(pipeNormVec1, pipeNormMag1)
      activeLightDir := normStage2(pipeNormVec2, pipeNormMag2)
      state := State.shadowSetup
    }
    
    // Build the shadow ray after normalized inputs are registered.
    is(State.shadowSetup) {
      shadowRay.origin := hitPoint + (hitNormal * EPSILON)
      shadowRay.dir := activeLightDir
      state := State.shadowPrep
    }
    
    // Shadow-intersection stage 1: coefficients and discriminant.
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

    // Shadow-intersection stage 2: solve candidate t.
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

    // Shadow-intersection stage 3: evaluate and advance shadow loop.
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
    
    // Accumulate diffuse and specular lighting for the visible light source.
    is(State.shade) {
      val light = io.lightData // Active light contributing to shading.
      
      when(!inShadow) {
        val diff = hitNormal.dot(activeLightDir) // Raw Lambertian dot product.
        val diffuse = maxFixed(diff, ZERO) // Clamped diffuse term.
        val diffuseContribution = light.intensity * diffuse // Diffuse lighting contribution.
        val nextIntensity = totalIntensity + diffuseContribution // Updated accumulated intensity.
        
        totalIntensity := nextIntensity
        currentIntensity := minFixed(nextIntensity, ONE)
      }.otherwise {
        currentIntensity := minFixed(totalIntensity, ONE)
      }
      
      state := State.output
    }
    
    // Map the final intensity to an ASCII character and advance to the next pixel.
    is(State.output) {
      val numChars = ASCII_CHARS.length.U // Number of characters in the ASCII ramp.
      val intensityVal = minFixed(currentIntensity, ONE).value // Clamped fixed-point intensity value.
      when(intensityVal > ZERO.value) {
        // printf(p"Pixel (${pixelX}, ${pixelY}) intensity: ${intensityVal}\n")
      }
      
      val charIndex = (intensityVal * (numChars - 1.U)) >> FP_FRAC // Character index derived from the normalized intensity.
      val safeCharIndex = Mux(charIndex >= numChars, numChars - 1.U, charIndex)(log2Ceil(ASCII_CHARS.length) - 1, 0) // Bounded index into the ASCII ramp.
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
// Verilog Generator
//=============================================================================
object EmitAsciiRayTracer extends App { // Standalone generator entry point.
  println("Generating SystemVerilog for AsciiRayTracer...")

  emitVerilog(
    new AsciiRayTracer(1),
    Array(
      "--target-dir",
      "generated"
    )
  )

  println("Generated: generated/AsciiRayTracer.sv")
}
