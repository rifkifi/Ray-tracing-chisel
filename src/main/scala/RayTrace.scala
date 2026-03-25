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
  val specular = new Fixed(bitWidth, fracBits) // Specular reflection coefficient.
}

class Light(val bitWidth: Int = 32, val fracBits: Int = 16) extends Bundle { // Point light source definition.
  val position = new Vector3(bitWidth, fracBits) // Light position in world space.
  val intensity = new Fixed(bitWidth, fracBits) // Light intensity multiplier.
}

class Intersection(val bitWidth: Int = 32, val fracBits: Int = 16) extends Bundle { // Intersection result container.
  val hit = Bool() // True when a ray intersects an object.
  val t = new Fixed(bitWidth, fracBits) // Distance along the ray to the hit.
  val objectId = UInt(8.W) // Identifier of the intersected object.
  val hitPoint = new Vector3(bitWidth, fracBits) // World-space hit position.
  val normal = new Vector3(bitWidth, fracBits) // Surface normal at the hit point.
}

//=============================================================================
// ASCII Ray Tracer Main Module
//=============================================================================
class AsciiRayTracer(
    val width: Int = 80, // Output frame width in characters.
    val height: Int = 40, // Output frame height in characters.
    val maxSpheres: Int = 8, // Maximum number of spheres available through the sphere port.
    val maxLights: Int = 4, // Maximum number of lights available through the light port.
    val bitWidth: Int = 16 // Total fixed-point bit width used internally.
) extends Module { // Main hardware ASCII ray tracer module.
  
  val FP_WIDTH = bitWidth // Total width of fixed-point values.
  val FP_FRAC = bitWidth / 2 // Number of fractional bits in fixed-point values.
  val sphereAddrWidth = math.max(1, log2Ceil(maxSpheres)) // Address width for sphere indexing.
  val lightAddrWidth = math.max(1, log2Ceil(maxLights)) // Address width for light indexing.
  
  val io = IO(new Bundle { // External module interface.
    val cameraPos = Input(new Vector3(FP_WIDTH, FP_FRAC)) // Camera position input.
    val cameraDir = Input(new Vector3(FP_WIDTH, FP_FRAC)) // Camera forward direction input.
    val fov = Input(UInt(FP_WIDTH.W)) // Field-of-view input in degrees.
    
    val sphereAddr = Output(UInt(sphereAddrWidth.W)) // Address used to fetch the current sphere.
    val sphereData = Input(new Sphere(FP_WIDTH, FP_FRAC)) // Sphere data returned for the selected address.
    val lightAddr = Output(UInt(lightAddrWidth.W)) // Address used to fetch the current light.
    val lightData = Input(new Light(FP_WIDTH, FP_FRAC)) // Light data returned for the selected address.
    
    val start = Input(Bool()) // Starts rendering a new frame.
    val busy = Output(Bool()) // Indicates the renderer is actively processing.
    val frameComplete = Output(Bool()) // Pulses when the last pixel of a frame is emitted.
    
    val asciiChar = Output(UInt(8.W)) // ASCII code for the current output pixel.
    val asciiValid = Output(Bool()) // Valid signal for the ASCII output.
    val x = Output(UInt(log2Ceil(width).W)) // X coordinate of the current emitted pixel.
    val y = Output(UInt(log2Ceil(height).W)) // Y coordinate of the current emitted pixel.
  })
  
  val ZERO = Fixed.zero(FP_WIDTH, FP_FRAC) // Fixed-point zero constant.
  val ONE = Fixed.one(FP_WIDTH, FP_FRAC) // Fixed-point one constant.
  val TWO = Fixed.fromDouble(2.0, FP_WIDTH, FP_FRAC) // Fixed-point two constant.
  val AMBIENT = Fixed.fromDouble(0.1, FP_WIDTH, FP_FRAC) // Ambient light floor.
  val EPSILON = Fixed.fromDouble(0.001, FP_WIDTH, FP_FRAC) // Small offset to avoid self-intersection.
  val INFINITY_VAL = Fixed.fromDouble(999999.0, FP_WIDTH, FP_FRAC) // Large sentinel distance for nearest-hit search.
  
  val ASCII_CHARS = VecInit(Seq( // ASCII ramp used to map intensity to characters.
    ' '.U, '.'.U, ','.U, ':'.U, ';'.U, 'o'.U, 'x'.U, '%'.U, '#'.U, '@'.U
  ))
  
  object State extends ChiselEnum { // Finite-state machine states for the renderer.
    val idle, initCamera, generateRay, intersect, lighting, shadowIntersect, shade, output = Value // Ordered rendering pipeline states.
  }
  val state = RegInit(State.idle) // Current FSM state.
  
  val pixelX = RegInit(0.U(log2Ceil(width).W)) // Current pixel X position.
  val pixelY = RegInit(0.U(log2Ceil(height).W)) // Current pixel Y position.
  
  val currentRay = RegInit(0.U.asTypeOf(new Ray(FP_WIDTH, FP_FRAC))) // Primary ray for the current pixel.
  val shadowRay = RegInit(0.U.asTypeOf(new Ray(FP_WIDTH, FP_FRAC))) // Shadow ray toward the active light.
  
  val currentIntensity = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC)) // Final intensity selected for ASCII output.
  val totalIntensity = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC)) // Accumulated lighting intensity for the hit point.
  
  val cameraForward = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))) // Normalized camera forward axis.
  val cameraRight = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))) // Normalized camera right axis.
  val cameraUp = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))) // Normalized camera up axis.
  val screenWidth = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC)) // Half-screen width in camera space.
  val screenHeight = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC)) // Half-screen height in camera space.
  
  val sphereIdx = RegInit(0.U(sphereAddrWidth.W)) // Index of the sphere being tested for primary intersection.
  val shadowSphereIdx = RegInit(0.U(sphereAddrWidth.W)) // Index of the sphere being tested for shadow intersection.
  val lightIdx = RegInit(0.U(lightAddrWidth.W)) // Index of the light currently being processed.
  
  val closestT = RegInit(INFINITY_VAL) // Distance to the closest sphere hit found so far.
  val closestObj = RegInit(0.U(8.W)) // Index of the closest intersected sphere.
  val closestCenter = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))) // Center of the closest intersected sphere.
  val closestSpecular = RegInit(Fixed.zero(FP_WIDTH, FP_FRAC)) // Specular coefficient of the closest intersected sphere.
  val allTested = RegInit(false.B) // True when all spheres have been checked for the current ray.
  val inShadow = RegInit(false.B) // True when the active light is blocked.
  
  val hitPoint = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))) // Surface point hit by the primary ray.
  val hitNormal = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))) // Surface normal at the hit point.
  val activeLightDir = RegInit(0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))) // Direction from the hit point to the active light.
  
  def cross(a: Vector3, b: Vector3): Vector3 = { // Computes the vector cross product.
    val res = Wire(new Vector3(FP_WIDTH, FP_FRAC)) // Output wire for the cross-product result.
    res.x := (a.y * b.z) - (a.z * b.y)
    res.y := (a.z * b.x) - (a.x * b.z)
    res.z := (a.x * b.y) - (a.y * b.x)
    res
  }
  
  def intersectSphere(ray: Ray, sphere: Sphere): (Bool, Fixed) = { // Computes whether a ray hits a sphere and returns the nearest positive distance.
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
      val t2 = (-b + sqrtDisc) / (TWO * a) // Far intersection candidate.
      
      when(t1 > ZERO) {
        hit := true.B
        t := t1
      }.elsewhen(t2 > ZERO) {
        hit := true.B
        t := t2
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
  
  def reflect(lightDir: Vector3, normal: Vector3): Vector3 = { // Reflects a direction vector about a surface normal.
    val scale = TWO * normal.dot(lightDir) // Projection scale used by the reflection formula.
    ((normal * scale) - lightDir).normalize()
  }
  
  def maxFixed(a: Fixed, b: Fixed): Fixed = Mux(a > b, a, b) // Returns the larger of two fixed-point values.
  
  def minFixed(a: Fixed, b: Fixed): Fixed = Mux(a < b, a, b) // Returns the smaller of two fixed-point values.
  
  def powFixed(base: Fixed, exponent: Int): Fixed = { // Raises a fixed-point value to an integer exponent.
    var result = ONE // Running multiplication result.
    for (_ <- 0 until exponent) {
      result = result * base
    }
    result
  }
  
  io.busy := (state =/= State.idle)
  io.asciiValid := false.B
  io.frameComplete := false.B
  io.sphereAddr := Mux(state === State.shadowIntersect, shadowSphereIdx, sphereIdx)
  io.lightAddr := lightIdx
  io.asciiChar := ASCII_CHARS(0)
  io.x := 0.U
  io.y := 0.U
  
  switch(state) {
    // Wait for a new frame request before starting any camera or pixel work.
    is(State.idle) {
      when(io.start) {
        state := State.initCamera
      }
    }
    
    // Build the camera basis vectors and derive the screen dimensions from the FOV.
    is(State.initCamera) {
      val upWorld = Wire(new Vector3(FP_WIDTH, FP_FRAC)) // World up axis used to build the camera basis.
      upWorld.x := ZERO
      upWorld.y := ONE
      upWorld.z := ZERO
      
      val forward = io.cameraDir.normalize() // Normalized camera forward axis.
      val right = cross(upWorld, forward).normalize() // Normalized camera right axis.
      val up = cross(forward, right).normalize() // Normalized camera up axis.
      
      cameraForward := forward
      cameraRight := right
      cameraUp := up
      
      val fovRad = Wire(new Fixed(FP_WIDTH, FP_FRAC)) // Fixed-point field-of-view in radians.
      val piScaled = Fixed.rawBits(math.Pi, FP_WIDTH, FP_FRAC).U(FP_WIDTH.W) // Pi constant encoded in fixed-point raw bits.
      fovRad.value := ((io.fov * piScaled) / 180.U)(FP_WIDTH - 1, 0)
      
      val halfWidth = fovRad >> 1 // Half-angle used for the projection plane size.
      screenWidth := halfWidth * Fixed.fromDouble(width.toDouble / height.toDouble, FP_WIDTH, FP_FRAC)
      screenHeight := halfWidth
      pixelX := 0.U
      pixelY := 0.U
      state := State.generateRay
    }
    
    // Convert the current pixel coordinate into a normalized viewing ray.
    is(State.generateRay) {
      val u = Wire(new Fixed(FP_WIDTH, FP_FRAC)) // Normalized horizontal pixel coordinate.
      val v = Wire(new Fixed(FP_WIDTH, FP_FRAC)) // Normalized vertical pixel coordinate.
      
      u.value := (pixelX << FP_FRAC) / width.U
      v.value := (pixelY << FP_FRAC) / height.U
      
      val uCentered = u - Fixed.half(FP_WIDTH, FP_FRAC) // Horizontal coordinate shifted around the screen center.
      val vCentered = Fixed.half(FP_WIDTH, FP_FRAC) - v // Vertical coordinate shifted around the screen center.
      val rightComp = cameraRight * (uCentered * screenWidth) // Horizontal ray contribution.
      val upComp = cameraUp * (vCentered * screenHeight) // Vertical ray contribution.
      val dir = cameraForward + rightComp + upComp // Unnormalized ray direction through the current pixel.
      val dirNorm = dir.normalize() // Normalized primary ray direction.
      
      currentRay.origin := io.cameraPos
      currentRay.dir := dirNorm
      
      sphereIdx := 0.U
      closestT := INFINITY_VAL
      closestObj := 0.U
      closestCenter := 0.U.asTypeOf(new Vector3(FP_WIDTH, FP_FRAC))
      closestSpecular := ZERO
      allTested := false.B
      currentIntensity := ZERO
      
      state := State.intersect
    }
    
    // Test the generated ray against every sphere and keep the closest hit.
    is(State.intersect) {
      when(!allTested) {
        val sphere = io.sphereData // Sphere currently fetched from external storage.
        val (hit, t) = intersectSphere(currentRay, sphere) // Intersection result for the current sphere.
        
        when(hit && t.value < closestT.value && t.value > ZERO.value) {
          // printf(p"Hit sphere ${sphereIdx} at t=${t}\n")
          closestT := t
          closestObj := sphereIdx
          closestCenter := sphere.center
          closestSpecular := sphere.specular
        }
        
        when(sphereIdx === (maxSpheres - 1).U) {
          allTested := true.B
        }.otherwise {
          sphereIdx := sphereIdx + 1.U
        }
      }.otherwise {
        when(closestT.value < INFINITY_VAL.value) {
          val nextHitPoint = currentRay.origin + (currentRay.dir * closestT) // Computed closest world-space hit position.
          hitPoint := nextHitPoint
          hitNormal := (nextHitPoint - closestCenter).normalize()
          totalIntensity := AMBIENT
          lightIdx := 0.U
          state := State.lighting
        }.otherwise {
          currentIntensity := ZERO
          state := State.output
        }
      }
    }
    
    // Step through each light source and prepare a shadow ray for visibility testing.
    is(State.lighting) {
      when(lightIdx < maxLights.U) {
        val light = io.lightData // Light currently fetched from external storage.
        val lightDir = (light.position - hitPoint).normalize() // Direction from the hit point to the light.
        
        shadowRay.origin := hitPoint + (hitNormal * EPSILON)
        shadowRay.dir := lightDir
        activeLightDir := lightDir
        shadowSphereIdx := 0.U
        inShadow := false.B
        state := State.shadowIntersect
      }.otherwise {
        currentIntensity := minFixed(totalIntensity, ONE)
        state := State.output
      }
    }
    
    // Check whether any sphere blocks the current light from reaching the hit point.
    is(State.shadowIntersect) {
      val sphere = io.sphereData // Sphere currently being tested against the shadow ray.
      val (shadowHit, shadowT) = intersectSphere(shadowRay, sphere) // Shadow-ray intersection result.
      val blocksLight = (shadowSphereIdx =/= closestObj) && shadowHit && (shadowT > EPSILON) // True when another sphere occludes the light.
      
      when(blocksLight) {
        inShadow := true.B
        state := State.shade
      }.elsewhen(shadowSphereIdx === (maxSpheres - 1).U) {
        state := State.shade
      }.otherwise {
        shadowSphereIdx := shadowSphereIdx + 1.U
      }
    }
    
    // Accumulate diffuse and specular lighting for the visible light source.
    is(State.shade) {
      val light = io.lightData // Active light contributing to shading.
      
      when(!inShadow) {
        val diff = hitNormal.dot(activeLightDir) // Raw Lambertian dot product.
        val diffuse = maxFixed(diff, ZERO) // Clamped diffuse term.
        val diffuseContribution = light.intensity * diffuse // Diffuse lighting contribution.
        
        val viewDir = (currentRay.origin - hitPoint).normalize() // Direction from the hit point back to the camera.
        val reflectDir = reflect(activeLightDir, hitNormal) // Reflected light direction about the surface normal.
        val specBase = maxFixed(viewDir.dot(reflectDir), ZERO) // Clamped specular base term.
        val specularPower = powFixed(specBase, 30) // Sharpened specular highlight term.
        val specularContribution = light.intensity * specularPower * closestSpecular // Specular lighting contribution.
        val nextIntensity = totalIntensity + diffuseContribution + specularContribution // Updated accumulated intensity.
        
        totalIntensity := nextIntensity
      }
      
      lightIdx := lightIdx + 1.U
      state := State.lighting
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
      
      when(pixelX === (width - 1).U && pixelY === (height - 1).U) {
        state := State.idle
        io.frameComplete := true.B
      }.elsewhen(pixelX === (width - 1).U) {
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
    new AsciiRayTracer(80, 40, 8, 4),
    Array(
      "--target-dir",
      "generated"
    )
  )

  println("Generated: generated/AsciiRayTracer.sv")
}
