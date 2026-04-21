import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer

case class SphereInput(cx: Double, cy: Double, cz: Double, radius: Double)
case class LightInput(px: Double, py: Double, pz: Double, intensity: Double)

object RayTracerTestSupport {
  private val FpWidth = 16
  private val FpFrac = 8
  private val AsciiChars = " .,:;ox%#@"
  private val EmptySphere = SphereInput(0.0, 0.0, 0.0, 0.0)
  val Width = 80
  val Height = 40

  def configureClockTimeout(clock: Clock, width: Int, height: Int, maxSpheres: Int, frames: Int = 1): Unit = {
    val cyclesPerPixel = (5 * maxSpheres) + 24
    val timeoutCycles = math.max(1000, frames * (width * height * cyclesPerPixel + 256))
    clock.setTimeout(timeoutCycles)
  }

  def fixedBits(value: Double): BigInt =
    Fixed.rawBits(value, FpWidth, FpFrac)

  def pokeSphere(target: Sphere, sphere: SphereInput): Unit = {
    target.center.x.value.poke(fixedBits(sphere.cx).U(FpWidth.W))
    target.center.y.value.poke(fixedBits(sphere.cy).U(FpWidth.W))
    target.center.z.value.poke(fixedBits(sphere.cz).U(FpWidth.W))
    target.radius.value.poke(fixedBits(sphere.radius).U(FpWidth.W))
    target.radiusSq.value.poke(fixedBits(sphere.radius * sphere.radius).U(FpWidth.W))
  }

  def pokeLight(target: Light, light: LightInput): Unit = {
    target.position.x.value.poke(fixedBits(light.px).U(FpWidth.W))
    target.position.y.value.poke(fixedBits(light.py).U(FpWidth.W))
    target.position.z.value.poke(fixedBits(light.pz).U(FpWidth.W))
    target.intensity.value.poke(fixedBits(light.intensity).U(FpWidth.W))
  }

  private def collectFrame(
      clock: Clock,
      asciiValid: Bool,
      asciiChar: UInt,
      y: UInt,
      frameComplete: Bool
  ): Seq[String] = {
    var currentRow = new StringBuilder
    var currentY = 0
    val frameRows = ArrayBuffer.empty[String]
    var done = false

    while (!done) {
      clock.step()

      if (asciiValid.peek().litToBoolean) {
        val pixelY = y.peek().litValue.toInt
        val pixelChar = asciiChar.peek().litValue.toInt.toChar

        if (pixelY != currentY) {
          frameRows += currentRow.toString
          currentRow.clear()
          currentY = pixelY
        }

        currentRow.append(pixelChar)
      }

      done = frameComplete.peek().litToBoolean
    }

    frameRows += currentRow.toString
    frameRows.toSeq
  }

  def renderHardcodedFrame(dut: AsciiRayTracerHardcoded): Seq[String] = {
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
    collectFrame(dut.clock, dut.io.asciiValid, dut.io.asciiChar, dut.io.y, dut.io.frameComplete)
  }

  def renderCoreFrame(
      dut: AsciiRayTracerCore,
      spheres: Seq[SphereInput],
      light: LightInput
  ): Seq[String] = {
    def driveScene(): Unit = {
      val sphereIdx = dut.io.sphereAddr.peek().litValue.toInt
      val sphere = spheres.lift(sphereIdx).getOrElse(EmptySphere)
      pokeSphere(dut.io.sphereData, sphere)
      pokeLight(dut.io.lightData, light)
    }

    driveScene()
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)

    var currentRow = new StringBuilder
    var currentY = 0
    val frameRows = ArrayBuffer.empty[String]
    var done = false

    while (!done) {
      driveScene()
      dut.clock.step()

      if (dut.io.asciiValid.peek().litToBoolean) {
        val pixelY = dut.io.y.peek().litValue.toInt
        val pixelChar = dut.io.asciiChar.peek().litValue.toInt.toChar

        if (pixelY != currentY) {
          frameRows += currentRow.toString
          currentRow.clear()
          currentY = pixelY
        }

        currentRow.append(pixelChar)
      }

      done = dut.io.frameComplete.peek().litToBoolean
    }

    frameRows += currentRow.toString
    frameRows.toSeq
  }

  def renderCoreWindow(
      dut: AsciiRayTracerCore,
      spheres: Seq[SphereInput],
      light: LightInput,
      xStart: Int,
      xEnd: Int,
      yStart: Int,
      yEnd: Int
  ): Seq[String] = {
    def driveScene(): Unit = {
      val sphereIdx = dut.io.sphereAddr.peek().litValue.toInt
      val sphere = spheres.lift(sphereIdx).getOrElse(EmptySphere)
      pokeSphere(dut.io.sphereData, sphere)
      pokeLight(dut.io.lightData, light)
    }

    val frame = Array.fill(yEnd - yStart + 1, xEnd - xStart + 1)(' ')
    driveScene()
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)

    var captureDone = false
    var frameDone = false
    while (!frameDone) {
      driveScene()
      dut.clock.step()

      if (dut.io.asciiValid.peek().litToBoolean) {
        val pixelX = dut.io.x.peek().litValue.toInt
        val pixelY = dut.io.y.peek().litValue.toInt
        val pixelChar = dut.io.asciiChar.peek().litValue.toInt.toChar

        if (pixelX >= xStart && pixelX <= xEnd && pixelY >= yStart && pixelY <= yEnd) {
          frame(pixelY - yStart)(pixelX - xStart) = pixelChar
        }

        if (!captureDone) {
          captureDone = pixelY > yEnd || (pixelY == yEnd && pixelX >= xEnd)
        }
      }

      frameDone = dut.io.frameComplete.peek().litToBoolean
    }

    frame.map(_.mkString).toSeq
  }

  def nonSpaceCount(frame: Seq[String]): Int =
    frame.map(_.count(_ != ' ')).sum

  def brightnessScore(frame: Seq[String]): Int =
    frame.iterator.flatMap(_.iterator).map(ch => AsciiChars.indexOf(ch).max(0)).sum

  def printFrame(title: String, frame: Seq[String]): Unit = {
    println("=" * 80)
    println(title)
    println("=" * 80)
    frame.foreach(println)
    println("=" * 80)
  }

  def packedWord(low16: BigInt, high16: BigInt): BigInt =
    (high16 << 16) | low16

  def sphereWords(sphere: SphereInput): Seq[BigInt] = Seq(
    packedWord(fixedBits(sphere.cx), fixedBits(sphere.cy)),
    packedWord(fixedBits(sphere.cz), fixedBits(sphere.radius)),
    fixedBits(sphere.radius * sphere.radius)
  )

  def lightWords(light: LightInput): Seq[BigInt] = Seq(
    packedWord(fixedBits(light.px), fixedBits(light.py)),
    packedWord(fixedBits(light.pz), fixedBits(light.intensity))
  )

  def writeMemWord(dut: AsciiRayTracerMem, addr: Int, data: BigInt): Unit = {
    dut.io.memWriteEn.poke(true.B)
    dut.io.memWriteMask.poke("b1111".U)
    dut.io.memWriteAddr.poke(addr.U)
    dut.io.memWriteData.poke(data.U(32.W))
    dut.io.memReadAddr.poke(0.U)
    dut.clock.step()
    dut.io.memWriteEn.poke(false.B)
    dut.clock.step()
  }

  def loadSceneIntoMem(dut: AsciiRayTracerMem, spheres: Seq[SphereInput], light: LightInput): Unit = {
    spheres.zipWithIndex.foreach { case (sphere, idx) =>
      sphereWords(sphere).zipWithIndex.foreach { case (word, wordIdx) =>
        writeMemWord(dut, idx * 3 + wordIdx, word)
      }
    }
    lightWords(light).zipWithIndex.foreach { case (word, wordIdx) =>
      writeMemWord(dut, spheres.length * 3 + wordIdx, word)
    }
  }

  def renderMemFrame(dut: AsciiRayTracerMem): Seq[String] = {
    dut.io.memWriteEn.poke(false.B)
    dut.io.memWriteMask.poke(0.U)
    dut.io.memWriteAddr.poke(0.U)
    dut.io.memWriteData.poke(0.U)
    dut.io.memReadAddr.poke(0.U)
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
    collectFrame(dut.clock, dut.io.asciiValid, dut.io.asciiChar, dut.io.y, dut.io.frameComplete)
  }
}

class AsciiRayTracerHardcodedTest extends AnyFlatSpec with ChiselScalatestTester {
  "AsciiRayTracerHardcoded" should "render a non-empty frame" in {
    test(new AsciiRayTracerHardcoded(1)) { dut =>
      RayTracerTestSupport.configureClockTimeout(dut.clock, width = 80, height = 40, maxSpheres = 3)

      val frame = RayTracerTestSupport.renderHardcodedFrame(dut)
      RayTracerTestSupport.printFrame("AsciiRayTracerHardcoded", frame)

      assert(frame.length == 40)
      assert(frame.forall(_.length == 80))
      assert(RayTracerTestSupport.nonSpaceCount(frame) > 0)
    }
  }
}

class AsciiRayTracerSphereIoTest extends AnyFlatSpec with ChiselScalatestTester {
  "AsciiRayTracerCore sphere IO" should "change the rendered frame when spheres are provided" in {
    test(new AsciiRayTracerCore(1)) { dut =>
      RayTracerTestSupport.configureClockTimeout(dut.clock, width = 80, height = 40, maxSpheres = 1, frames = 2)

      val light = LightInput(0.0, 5.0, 5.0, 0.5)
      val emptyFrame = RayTracerTestSupport.renderCoreWindow(
        dut,
        Seq(SphereInput(0.0, 0.0, 0.0, 0.0)),
        light,
        xStart = 12,
        xEnd = 67,
        yStart = 10,
        yEnd = 30
      )
      val sphereFrame = RayTracerTestSupport.renderCoreWindow(
        dut,
        Seq(SphereInput(0.0, -0.2, -1.9, 0.7)),
        light,
        xStart = 12,
        xEnd = 67,
        yStart = 10,
        yEnd = 30
      )
      RayTracerTestSupport.printFrame("AsciiRayTracerCore Sphere IO Empty Window", emptyFrame)
      RayTracerTestSupport.printFrame("AsciiRayTracerCore Sphere IO Populated Window", sphereFrame)

      assert(RayTracerTestSupport.nonSpaceCount(emptyFrame) == 0)
      assert(RayTracerTestSupport.nonSpaceCount(sphereFrame) > 0)
      assert(emptyFrame != sphereFrame)
    }
  }
}

class AsciiRayTracerLightIoTest extends AnyFlatSpec with ChiselScalatestTester {
  "AsciiRayTracerCore light IO" should "change scene brightness when light intensity changes" in {
    test(new AsciiRayTracerCore(1)) { dut =>
      RayTracerTestSupport.configureClockTimeout(dut.clock, width = 80, height = 40, maxSpheres = 1, frames = 2)

      val sphere = Seq(SphereInput(0.0, -0.2, -1.9, 0.7))
      val darkLight = LightInput(0.0, 5.0, 5.0, 0.0)
      val brightLight = LightInput(0.0, 5.0, 5.0, 0.5)

      val darkFrame = RayTracerTestSupport.renderCoreWindow(
        dut,
        sphere,
        darkLight,
        xStart = 12,
        xEnd = 67,
        yStart = 10,
        yEnd = 30
      )
      val brightFrame = RayTracerTestSupport.renderCoreWindow(
        dut,
        sphere,
        brightLight,
        xStart = 12,
        xEnd = 67,
        yStart = 10,
        yEnd = 30
      )
      RayTracerTestSupport.printFrame("AsciiRayTracerCore Light IO Dark Window", darkFrame)
      RayTracerTestSupport.printFrame("AsciiRayTracerCore Light IO Bright Window", brightFrame)

      assert(RayTracerTestSupport.nonSpaceCount(brightFrame) > 0)
      assert(RayTracerTestSupport.brightnessScore(brightFrame) > RayTracerTestSupport.brightnessScore(darkFrame))
      assert(darkFrame != brightFrame)
    }
  }
}

class AsciiRayTracerMemTest extends AnyFlatSpec with ChiselScalatestTester {
  "AsciiRayTracerMem" should "load sphere and light data from memory and render a non-empty frame" in {
    test(new AsciiRayTracerMem(1, useMemModel = true)) { dut =>
      RayTracerTestSupport.configureClockTimeout(dut.clock, width = 80, height = 40, maxSpheres = 1)

      RayTracerTestSupport.loadSceneIntoMem(
        dut,
        spheres = Seq(SphereInput(0.0, -0.2, -1.9, 0.7)),
        light = LightInput(0.0, 5.0, 5.0, 0.5)
      )

      val frame = RayTracerTestSupport.renderMemFrame(dut)
      RayTracerTestSupport.printFrame("AsciiRayTracerMem", frame)

      assert(frame.length == 40)
      assert(frame.forall(_.length == 80))
      assert(RayTracerTestSupport.nonSpaceCount(frame) > 0)
    }
  }
}
