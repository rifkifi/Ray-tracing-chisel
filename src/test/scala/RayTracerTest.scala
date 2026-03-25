import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import java.io._
import scala.collection.mutable.ArrayBuffer

case class SphereModel(cx: Double, cy: Double, cz: Double, radius: Double, specular: Double)
case class LightModel(px: Double, py: Double, pz: Double, intensity: Double)

object TestSceneDriver {
  private def fixedBits(d: Double, bitWidth: Int, fracBits: Int): BigInt = {
    val scaled = BigDecimal(d) * BigDecimal(1 << fracBits)
    val raw = scaled.toBigInt
    val min = -(BigInt(1) << (bitWidth - 1))
    val max = (BigInt(1) << (bitWidth - 1)) - 1
    val clamped = raw.max(min).min(max)
    if (clamped < 0) clamped + (BigInt(1) << bitWidth) else clamped
  }

  def pokeFixed(port: UInt, value: Double): Unit = {
    val bitWidth = port.getWidth
    val fracBits = bitWidth / 2
    port.poke(fixedBits(value, bitWidth, fracBits).U(bitWidth.W))
  }

  private def driveSphere(dut: AsciiRayTracer, sphere: SphereModel): Unit = {
    pokeFixed(dut.io.sphereData.center.x.value, sphere.cx)
    pokeFixed(dut.io.sphereData.center.y.value, sphere.cy)
    pokeFixed(dut.io.sphereData.center.z.value, sphere.cz)
    pokeFixed(dut.io.sphereData.radius.value, sphere.radius)
    pokeFixed(dut.io.sphereData.radiusSq.value, sphere.radius * sphere.radius)
    pokeFixed(dut.io.sphereData.specular.value, sphere.specular)
  }

  private def driveLight(dut: AsciiRayTracer, light: LightModel): Unit = {
    pokeFixed(dut.io.lightData.position.x.value, light.px)
    pokeFixed(dut.io.lightData.position.y.value, light.py)
    pokeFixed(dut.io.lightData.position.z.value, light.pz)
    pokeFixed(dut.io.lightData.intensity.value, light.intensity)
  }

  def drive(dut: AsciiRayTracer, spheres: Seq[SphereModel], lights: Seq[LightModel]): Unit = {
    val sphere = spheres.lift(dut.io.sphereAddr.peek().litValue.toInt).getOrElse(SphereModel(0.0, 0.0, 0.0, 0.0, 0.0))
    val light = lights.lift(dut.io.lightAddr.peek().litValue.toInt).getOrElse(LightModel(0.0, 0.0, 0.0, 0.0))
    // println(s"Driving sphere at address ${dut.io.sphereAddr.peek().litValue} with center=(${sphere.cx}, ${sphere.cy}, ${sphere.cz}), radius=${sphere.radius}")
    driveSphere(dut, sphere)
    driveLight(dut, light)
  }
}

object RayTracerTestSupport {
  def configureClockTimeout(
      dut: AsciiRayTracer,
      width: Int,
      height: Int,
      maxSpheres: Int,
      maxLights: Int,
      frames: Int = 1
  ): Unit = {
    // Worst case per pixel includes:
    // ray generation + sphere scan + hit finalize + per-light shadow/shade + output.
    val cyclesPerPixel = maxSpheres + 4 + maxLights * (maxSpheres + 2)
    val timeoutCycles = math.max(1000, frames * (width * height * cyclesPerPixel + 16))
    dut.clock.setTimeout(timeoutCycles)
  }
}

object SoftwareRayTracerModel {
  final case class Vec3(x: Double, y: Double, z: Double) {
    def +(that: Vec3): Vec3 = Vec3(x + that.x, y + that.y, z + that.z)
    def -(that: Vec3): Vec3 = Vec3(x - that.x, y - that.y, z - that.z)
    def *(scalar: Double): Vec3 = Vec3(x * scalar, y * scalar, z * scalar)
    def dot(that: Vec3): Double = x * that.x + y * that.y + z * that.z
    def lengthSq: Double = dot(this)
  }

  private final case class Ray(origin: Vec3, dir: Vec3)

  private val Ambient = 0.1
  private val Epsilon = 0.001
  private val InfinityVal = 999999.0
  private val AsciiChars = " .,:;ox%#@"

  private def normalize(v: Vec3): Vec3 = {
    val len = math.sqrt(v.lengthSq)
    if (len > 0.0) Vec3(v.x / len, v.y / len, v.z / len) else v
  }

  private def cross(a: Vec3, b: Vec3): Vec3 =
    Vec3(
      a.y * b.z - a.z * b.y,
      a.z * b.x - a.x * b.z,
      a.x * b.y - a.y * b.x
    )

  private def reflect(lightDir: Vec3, normal: Vec3): Vec3 = {
    val scale = 2.0 * normal.dot(lightDir)
    normalize((normal * scale) - lightDir)
  }

  private def powFixed(base: Double, exponent: Int): Double = {
    var result = 1.0
    for (_ <- 0 until exponent) {
      result *= base
    }
    result
  }

  private def intersectSphere(ray: Ray, sphere: SphereModel): Option[Double] = {
    val center = Vec3(sphere.cx, sphere.cy, sphere.cz)
    val oc = ray.origin - center
    val a = ray.dir.dot(ray.dir)
    val b = oc.dot(ray.dir) * 2.0
    val c = oc.dot(oc) - sphere.radius * sphere.radius
    val discriminant = (b * b) - ((2.0 * 2.0) * a * c)

    if (discriminant > 0.0) {
      val sqrtDisc = discriminant / 2.0
      val t1 = (-b - sqrtDisc) / (2.0 * a)
      val t2 = (-b + sqrtDisc) / (2.0 * a)
      if (t1 > 0.0) Some(t1)
      else if (t2 > 0.0) Some(t2)
      else None
    } else {
      None
    }
  }

  def renderFrame(
      width: Int,
      height: Int,
      spheres: Seq[SphereModel],
      lights: Seq[LightModel],
      cameraPos: Vec3,
      cameraDir: Vec3,
      fovDeg: Double
  ): Seq[String] = {
    val forward = normalize(cameraDir)
    val right = normalize(cross(Vec3(0.0, 1.0, 0.0), forward))
    val up = normalize(cross(forward, right))

    val fovRad = (fovDeg * math.Pi) / 180.0
    val halfWidth = fovRad / 2.0
    val screenWidth = halfWidth * (width.toDouble / height.toDouble)
    val screenHeight = halfWidth

    (0 until height).map { y =>
      val row = new StringBuilder
      for (x <- 0 until width) {
        val u = x.toDouble / width.toDouble
        val v = y.toDouble / height.toDouble
        val uCentered = u - 0.5
        val vCentered = 0.5 - v
        val rightComp = right * (uCentered * screenWidth)
        val upComp = up * (vCentered * screenHeight)
        val ray = Ray(cameraPos, normalize(forward + rightComp + upComp))

        var closestT = InfinityVal
        var closestObj = -1
        var closestCenter = Vec3(0.0, 0.0, 0.0)
        var closestSpecular = 0.0

        for ((sphere, idx) <- spheres.zipWithIndex) {
          intersectSphere(ray, sphere).foreach { t =>
            if (t < closestT && t > 0.0) {
              closestT = t
              closestObj = idx
              closestCenter = Vec3(sphere.cx, sphere.cy, sphere.cz)
              closestSpecular = sphere.specular
            }
          }
        }

        val currentIntensity =
          if (closestT < InfinityVal) {
            val hitPoint = ray.origin + (ray.dir * closestT)
            val hitNormal = normalize(hitPoint - closestCenter)
            var totalIntensity = Ambient

            for (light <- lights) {
              val lightPos = Vec3(light.px, light.py, light.pz)
              val lightDir = normalize(lightPos - hitPoint)
              val shadowRay = Ray(hitPoint + (hitNormal * Epsilon), lightDir)

              var inShadow = false
              var shadowSphereIdx = 0
              while (shadowSphereIdx < spheres.length && !inShadow) {
                val blocksLight =
                  shadowSphereIdx != closestObj &&
                    intersectSphere(shadowRay, spheres(shadowSphereIdx)).exists(_ > Epsilon)
                if (blocksLight) {
                  inShadow = true
                }
                shadowSphereIdx += 1
              }

              if (!inShadow) {
                val diff = hitNormal.dot(lightDir)
                val diffuse = math.max(diff, 0.0)
                val diffuseContribution = light.intensity * diffuse

                val viewDir = normalize(ray.origin - hitPoint)
                val reflectDir = reflect(lightDir, hitNormal)
                val specBase = math.max(viewDir.dot(reflectDir), 0.0)
                val specularPower = powFixed(specBase, 30)
                val specularContribution = light.intensity * specularPower * closestSpecular

                totalIntensity += diffuseContribution + specularContribution
              }
            }

            math.min(totalIntensity, 1.0)
          } else {
            0.0
          }

        val charIndex = math.min((currentIntensity * (AsciiChars.length - 1)).toInt, AsciiChars.length - 1)
        row.append(AsciiChars(charIndex))
      }
      row.toString
    }
  }
}

// Test for the AsciiRayTracer module
class AsciiRayTracerTest extends AnyFlatSpec with ChiselScalatestTester {
  
  "AsciiRayTracer" should "render a simple frame" in {
    test(new AsciiRayTracer(80, 40, 3, 1, 16)) { dut =>
      RayTracerTestSupport.configureClockTimeout(dut, width = 80, height = 40, maxSpheres = 1, maxLights = 1)
      print("\u001b[2J\u001b[H")  // Clear screen
      println("=" * 80)
      println("ASCII Ray Tracer - Hardware Simulation")
      println("=" * 80)
      println()
      
      // Configure camera
      TestSceneDriver.pokeFixed(dut.io.cameraPos.x.value, 0.0)
      TestSceneDriver.pokeFixed(dut.io.cameraPos.y.value, 0.0)
      TestSceneDriver.pokeFixed(dut.io.cameraPos.z.value, 2.0)
      
      TestSceneDriver.pokeFixed(dut.io.cameraDir.x.value, 0.0)
      TestSceneDriver.pokeFixed(dut.io.cameraDir.y.value, 0.0)
      TestSceneDriver.pokeFixed(dut.io.cameraDir.z.value, -1.0)
      
      dut.io.fov.poke(80.U(16.W))
      
      val spheres = Seq(
        SphereModel(0.0, -0.5, -1.0, 0.8, 1),
        SphereModel(1.2, -0.2, -2.0, 0.6, 0.7),
        SphereModel(-1.2, -0.1, -0.5, 0.5, 0.6),
        // SphereModel(0.0, -100.5, -3.0, 100.0, 0.1)
      )
      val lights = Seq(
        // LightModel(2.0, 2.0, -2.0, 0.7),
        // LightModel(-2.0, 3.0, -3.0, 0.5),
        LightModel(0.0, 5.0, 5, 0.5)
      )
      TestSceneDriver.drive(dut, spheres, lights)
      
      // Start rendering
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)
      
      // Collect output
      var currentRow = new StringBuilder
      var currentY = 0
      var totalPixels = 0
      val frameRows = ArrayBuffer.empty[String]
      
      while (!dut.io.frameComplete.peek().litToBoolean) {
        TestSceneDriver.drive(dut, spheres, lights)
        dut.clock.step()
        
        if (dut.io.asciiValid.peek().litToBoolean) {
          val y = dut.io.y.peek().litValue.toInt
          val asciiChar = dut.io.asciiChar.peek().litValue.toInt.toChar
          
          if (y != currentY) {
            println(currentRow.toString)
            frameRows += currentRow.toString
            currentRow.clear()
            currentY = y
          }
          
          currentRow.append(asciiChar)
          totalPixels += 1
        }
      }
      
      // Print last row
      println(currentRow.toString)
      frameRows += currentRow.toString
      println()
      println("=" * 80)
      println(s"Rendering Complete! Total pixels: $totalPixels")
      println("=" * 80)

      val filename = "hardware_raytrace.txt"
      val writer = new PrintWriter(new File(filename))
      frameRows.foreach(writer.println)
      writer.close()
      println(s"Output saved to: $filename")
    }
  }
}

// Test with animated camera
class AnimatedAsciiRayTracerTest extends AnyFlatSpec with ChiselScalatestTester {
  
  "Animated AsciiRayTracer" should "render rotating view" in {
    test(new AsciiRayTracer(80, 40, 3, 1, 16)) { dut =>
      val frames = 30
      RayTracerTestSupport.configureClockTimeout(dut, width = 80, height = 40, maxSpheres = 4, maxLights = 2, frames = frames)
      val spheres = Seq(
        SphereModel(0.0, -0.5, -3.0, 0.8, 0.5),
        SphereModel(1.2, -0.2, -4.0, 0.6, 0.7),
        SphereModel(-1.2, -0.1, -3.5, 0.5, 0.6),
        // SphereModel(0.0, -100.5, -3.0, 100.0, 0.1)
      )
      val lights = Seq(
        LightModel(2.0, 2.0, -2.0, 0.7),
        // LightModel(-2.0, 3.0, -3.0, 0.5),
        // LightModel(0.0, 5.0, -1.0, 0.4)
      )
      
      for (frame <- 0 until frames) {
        val angle = frame * 2 * Math.PI / frames
        val cameraX = 2.5 * math.cos(angle)
        val cameraZ = 2.5 * math.sin(angle)
        
        // Update camera
        TestSceneDriver.pokeFixed(dut.io.cameraPos.x.value, cameraX)
        TestSceneDriver.pokeFixed(dut.io.cameraPos.y.value, 0.5)
        TestSceneDriver.pokeFixed(dut.io.cameraPos.z.value, cameraZ)
        
        TestSceneDriver.pokeFixed(dut.io.cameraDir.x.value, -cameraX)
        TestSceneDriver.pokeFixed(dut.io.cameraDir.y.value, -0.1)
        TestSceneDriver.pokeFixed(dut.io.cameraDir.z.value, -cameraZ)
        
        dut.io.fov.poke(80.U(32.W))
        
        // Clear screen and show frame info
        print("\u001b[2J\u001b[H")
        println("=" * 80)
        println(s"Frame ${frame + 1}/$frames - Camera: (${"%.2f".format(cameraX)}, 0.5, ${"%.2f".format(cameraZ)})")
        println("=" * 80)
        println()
        
        // Render frame
        TestSceneDriver.drive(dut, spheres, lights)
        dut.io.start.poke(true.B)
        dut.clock.step()
        dut.io.start.poke(false.B)
        
        var currentRow = new StringBuilder
        var currentY = 0
        
        while (!dut.io.frameComplete.peek().litToBoolean) {
          TestSceneDriver.drive(dut, spheres, lights)
          dut.clock.step()
          
          if (dut.io.asciiValid.peek().litToBoolean) {
            val y = dut.io.y.peek().litValue.toInt
            val asciiChar = dut.io.asciiChar.peek().litValue.toInt.toChar
            
            if (y != currentY) {
              println(currentRow.toString)
              currentRow.clear()
              currentY = y
            }
            
            currentRow.append(asciiChar)
          }
        }
        
        println(currentRow.toString)
        println()
        println("=" * 80)
        
        Thread.sleep(100)  // Animation delay
      }
    }
  }
}

// Performance test
class PerformanceAsciiRayTracerTest extends AnyFlatSpec with ChiselScalatestTester {
  
  "Performance test" should "measure rendering speed" in {
    val resolutions = List(
      (40, 20, "Small"),
      (80, 40, "Medium")
    )
    
    println("=" * 80)
    println("Performance Test")
    println("=" * 80)
    println()
    
    for ((width, height, name) <- resolutions) {
      test(new AsciiRayTracer(width, height, 4, 2)) { dut =>
        RayTracerTestSupport.configureClockTimeout(dut, width = width, height = height, maxSpheres = 4, maxLights = 2)
        println(s"Testing $name Resolution: ${width}x${height}")
        val spheres = Seq(
          SphereModel(0.0, -0.5, -3.0, 0.8, 0.5)
        )
        val lights = Seq(
          LightModel(2.0, 2.0, -2.0, 0.7)
        )
        
        // Configure camera
        TestSceneDriver.pokeFixed(dut.io.cameraPos.x.value, 0.0)
        TestSceneDriver.pokeFixed(dut.io.cameraPos.y.value, 0.5)
        TestSceneDriver.pokeFixed(dut.io.cameraPos.z.value, 2.0)
        
        TestSceneDriver.pokeFixed(dut.io.cameraDir.x.value, 0.0)
        TestSceneDriver.pokeFixed(dut.io.cameraDir.y.value, -0.1)
        TestSceneDriver.pokeFixed(dut.io.cameraDir.z.value, -1.0)
        
        dut.io.fov.poke(80.U(32.W))
        
        // Start timing
        val startTime = System.nanoTime()
        
        // Render
        TestSceneDriver.drive(dut, spheres, lights)
        dut.io.start.poke(true.B)
        dut.clock.step()
        dut.io.start.poke(false.B)
        
        var pixelsProcessed = 0
        var cycles = 0
        
        while (!dut.io.frameComplete.peek().litToBoolean) {
          TestSceneDriver.drive(dut, spheres, lights)
          dut.clock.step()
          cycles += 1
          if (dut.io.asciiValid.peek().litToBoolean) {
            pixelsProcessed += 1
          }
        }
        
        val endTime = System.nanoTime()
        val duration = (endTime - startTime) / 1e9
        val fps = 1.0 / duration
        
        println(s"  Time: ${"%.3f".format(duration)} seconds")
        println(s"  FPS: ${"%.1f".format(fps)}")
        println(s"  Cycles: $cycles")
        println(s"  Cycles per pixel: ${cycles / pixelsProcessed}")
        println()
      }
    }
  }
}

// Software simulation test (doesn't require hardware)
class SoftwareSimulationTest extends AnyFlatSpec {
  
  "Software simulation" should "generate ASCII art" in {
    val width = 80
    val height = 40
    
    println("=" * 80)
    println("Software Simulation - ASCII Art Generation")
    println("=" * 80)
    println()
    
    val spheres = Seq(
      SphereModel(0.0, -0.5, -1.0, 0.8, 1.0),
      SphereModel(1.2, -0.2, -2.0, 0.6, 0.7),
      SphereModel(-1.2, -0.1, -0.5, 0.5, 0.6)
    )
    val lights = Seq(
      LightModel(0.0, 5.0, 5.0, 0.5)
    )

    val frameRows = SoftwareRayTracerModel.renderFrame(
      width = width,
      height = height,
      spheres = spheres,
      lights = lights,
      cameraPos = SoftwareRayTracerModel.Vec3(0.0, 0.0, 2.0),
      cameraDir = SoftwareRayTracerModel.Vec3(0.0, 0.0, -1.0),
      fovDeg = 80.0
    )

    frameRows.foreach(println)
    
    println()
    println("=" * 80)
    
    // Save to file
    val filename = "software_raytrace.txt"
    val writer = new PrintWriter(new File(filename))
    frameRows.foreach(writer.println)
    writer.close()
    
    println(s"Output saved to: $filename")
  }
}

// Main test runner
object RunAsciiRayTracerTests {
  def main(args: Array[String]): Unit = {
    println("\n" + "=" * 80)
    println("ASCII Ray Tracer Test Suite")
    println("=" * 80)
    println()
    
    println("Select test to run:")
    println("1. Software Simulation (Fast, no hardware)")
    println("2. Hardware Simulation (Single Frame)")
    println("3. Hardware Simulation (Animated)")
    println("4. Performance Test")
    println("5. Run All Tests")
    
    print("\nEnter choice (1-5): ")
    val choice = scala.io.StdIn.readLine().toInt
    
    choice match {
      case 1 =>
        println("\nRunning Software Simulation...\n")
        (new SoftwareSimulationTest).execute()
      case 2 =>
        println("\nRunning Hardware Simulation...\n")
        (new AsciiRayTracerTest).execute()
      case 3 =>
        println("\nRunning Animated Test...\n")
        (new AnimatedAsciiRayTracerTest).execute()
      case 4 =>
        println("\nRunning Performance Test...\n")
        (new PerformanceAsciiRayTracerTest).execute()
      case 5 =>
        println("\nRunning All Tests...\n")
        (new SoftwareSimulationTest).execute()
        println("\n" + "=" * 80 + "\n")
        (new AsciiRayTracerTest).execute()
      case _ =>
        println("Invalid choice")
    }
  }
}
