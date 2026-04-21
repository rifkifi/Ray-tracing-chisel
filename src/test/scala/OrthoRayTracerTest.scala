import chisel3._
import chisel3.util.log2Ceil
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

case class OrthoSphereInput(cx: Int, cy: Int, cz: Int, rSq: Int)
case class OrthoLightInput(x: Int, y: Int)

object OrthoTestSupport {
  val Width = 80
  val Height = 40
  val AsciiChars = IndexedSeq(' ', '.', ',', ':', ';', 'o', 'x', '%', '#', '@')

  def charRank(ch: Char): Int = AsciiChars.indexOf(ch)

  def patchBrightness(
      frame: Array[Array[Char]],
      centerX: Int,
      centerY: Int,
      radius: Int = 1
  ): Int = {
    val xs = (centerX - radius).max(0) to (centerX + radius).min(frame.head.length - 1)
    val ys = (centerY - radius).max(0) to (centerY + radius).min(frame.length - 1)
    ys.flatMap(y => xs.map(x => charRank(frame(y)(x)).max(0))).sum
  }

  def nonSpaceCount(frame: Array[Array[Char]]): Int =
    frame.iterator.map(_.count(_ != ' ')).sum

  def printFrame(title: String, frame: Array[Array[Char]]): Unit = {
    println("=" * 80)
    println(title)
    println("=" * 80)
    frame.map(_.mkString).foreach(println)
    println("=" * 80)
  }

  def configureClockTimeout(clock: Clock, maxSpheres: Int, frames: Int = 1): Unit = {
    val cyclesPerPixel = maxSpheres + 8
    val timeoutCycles = math.max(1000, frames * (Width * Height * cyclesPerPixel + 256))
    clock.setTimeout(timeoutCycles)
  }

  def startHardcodedFrame(dut: OrthoRayTracerHardcoded): Unit = {
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
  }

  def startCoreFrame(dut: OrthoRayTracerCore): Unit = {
    dut.io.start.poke(true.B)
    dut.io.configValid.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
  }

  def drainHardcodedFrame(dut: OrthoRayTracerHardcoded): Array[Array[Char]] = {
    val frame = Array.fill(Height, Width)(' ')
    val totalPixels = Width * Height

    (0 until totalPixels).foreach { _ =>
      while (!dut.io.asciiValid.peek().litToBoolean) {
        dut.clock.step()
      }
      val charOut = dut.io.asciiChar.peek().litValue.toInt.toChar
      val x = dut.io.x.peek().litValue.toInt
      val y = dut.io.y.peek().litValue.toInt
      frame(y)(x) = charOut
      dut.clock.step()
    }

    frame
  }

  def drainCoreFrame(dut: OrthoRayTracerCore): Array[Array[Char]] = {
    val frame = Array.fill(Height, Width)(' ')
    val totalPixels = Width * Height

    (0 until totalPixels).foreach { _ =>
      while (!dut.io.out.asciiValid.peek().litToBoolean) {
        dut.clock.step()
      }
      val charOut = dut.io.out.asciiChar.peek().litValue.toInt.toChar
      val x = dut.io.out.x.peek().litValue.toInt
      val y = dut.io.out.y.peek().litValue.toInt
      frame(y)(x) = charOut
      dut.clock.step()
    }

    frame
  }

  def drainMemFrame(dut: OrthoRayTracerMem): Array[Array[Char]] = {
    val frame = Array.fill(Height, Width)(' ')
    val totalPixels = Width * Height

    (0 until totalPixels).foreach { _ =>
      while (!dut.io.asciiValid.peek().litToBoolean) {
        dut.clock.step()
      }
      val charOut = dut.io.asciiChar.peek().litValue.toInt.toChar
      val x = dut.io.x.peek().litValue.toInt
      val y = dut.io.y.peek().litValue.toInt
      frame(y)(x) = charOut
      dut.clock.step()
    }

    frame
  }

  private def toTwosComplement(value: Int, bits: Int): BigInt = {
    val mod = BigInt(1) << bits
    val raw = BigInt(value)
    if (raw < 0) mod + raw else raw
  }

  def pokeCoreConfig(
      dut: OrthoRayTracerCore,
      spheres: Seq[OrthoSphereInput],
      light: OrthoLightInput
  ): Unit = {
    dut.io.config.numSpheres.poke(spheres.length.U)
    dut.io.config.lightX.poke(light.x.S)
    dut.io.config.lightY.poke(light.y.S)
    for (i <- 0 until dut.maxSpheres) {
      val sphere = spheres.lift(i).getOrElse(OrthoSphereInput(0, 0, 0, 0))
      dut.io.config.spheres(i).cx.poke(sphere.cx.S)
      dut.io.config.spheres(i).cy.poke(sphere.cy.S)
      dut.io.config.spheres(i).cz.poke(sphere.cz.S)
      dut.io.config.spheres(i).rSq.poke(sphere.rSq.U)
    }
  }

  def packedHeader(numSpheres: Int, light: OrthoLightInput, countBits: Int, lightBits: Int): BigInt =
    toTwosComplement(numSpheres, countBits) |
      (toTwosComplement(light.x, lightBits) << 8) |
      (toTwosComplement(light.y, lightBits) << 16)

  def packedSphereWord0(sphere: OrthoSphereInput, cxBits: Int, cyBits: Int): BigInt =
    toTwosComplement(sphere.cx, cxBits) |
      (toTwosComplement(sphere.cy, cyBits) << cxBits)

  def packedSphereWord1(sphere: OrthoSphereInput, czBits: Int): BigInt =
    toTwosComplement(sphere.cz, czBits) |
      (BigInt(sphere.rSq) << czBits)

  def writeMemWord(dut: OrthoRayTracerMem, addr: Int, data: BigInt): Unit = {
    dut.io.memWriteEn.poke(true.B)
    dut.io.memWriteMask.poke("b1111".U)
    dut.io.memWriteAddr.poke(addr.U)
    dut.io.memWriteData.poke(data.U(32.W))
    dut.io.memReadAddr.poke(0.U)
    dut.clock.step()
    dut.io.memWriteEn.poke(false.B)
    dut.clock.step()
  }

  def loadMemScene(
      dut: OrthoRayTracerMem,
      spheres: Seq[OrthoSphereInput],
      light: OrthoLightInput,
      cxBits: Int,
      cyBits: Int,
      czBits: Int,
      lightBits: Int
  ): Unit = {
    val countBits = math.max(1, log2Ceil(dut.maxSpheres + 1))
    writeMemWord(dut, 0, packedHeader(spheres.length, light, countBits, lightBits))
    spheres.zipWithIndex.foreach { case (sphere, idx) =>
      writeMemWord(dut, idx * 2 + 1, packedSphereWord0(sphere, cxBits, cyBits))
      writeMemWord(dut, idx * 2 + 2, packedSphereWord1(sphere, czBits))
    }
  }

  def startMemFrame(dut: OrthoRayTracerMem): Unit = {
    dut.io.memWriteEn.poke(false.B)
    dut.io.memWriteMask.poke(0.U)
    dut.io.memWriteAddr.poke(0.U)
    dut.io.memWriteData.poke(0.U)
    dut.io.memReadAddr.poke(0.U)
    dut.io.start.poke(true.B)
    dut.clock.step()
    dut.io.start.poke(false.B)
  }
}

class OrthoRayTracerHardcodedTest extends AnyFlatSpec with ChiselScalatestTester {
  private def dut = new OrthoRayTracerHardcoded(
    maxSpheres = 2,
    cxBits = 6,
    cyBits = 6,
    czBits = 5,
    rSqBits = 10,
    lightBits = 5
  )

  "OrthoRayTracerHardcoded" should "render the built-in scene" in {
    test(dut) { dut =>
      OrthoTestSupport.configureClockTimeout(dut.clock, maxSpheres = 2)

      OrthoTestSupport.startHardcodedFrame(dut)
      val frame = OrthoTestSupport.drainHardcodedFrame(dut)
      OrthoTestSupport.printFrame("OrthoRayTracerHardcoded", frame)

      assert(frame(20)(40) != ' ')
      assert(
        OrthoTestSupport.patchBrightness(frame, 44, 20, radius = 1) >
          OrthoTestSupport.patchBrightness(frame, 36, 20, radius = 1)
      )
    }
  }
}

class OrthoRayTracerSphereIoTest extends AnyFlatSpec with ChiselScalatestTester {
  "OrthoRayTracerCore sphere config" should "change the frame when spheres are enabled" in {
    test(new OrthoRayTracerCore(maxSpheres = 1, cxBits = 6, cyBits = 6, czBits = 5, rSqBits = 10, lightBits = 5)) {
      dut =>
        OrthoTestSupport.configureClockTimeout(dut.clock, maxSpheres = 1)

        OrthoTestSupport.pokeCoreConfig(dut, spheres = Seq.empty, light = OrthoLightInput(4, 0))
        OrthoTestSupport.startCoreFrame(dut)
        val emptyFrame = OrthoTestSupport.drainCoreFrame(dut)

        OrthoTestSupport.pokeCoreConfig(
          dut,
          spheres = Seq(OrthoSphereInput(0, 0, 0, 196)),
          light = OrthoLightInput(4, 0)
        )
        OrthoTestSupport.startCoreFrame(dut)
        val sphereFrame = OrthoTestSupport.drainCoreFrame(dut)
        OrthoTestSupport.printFrame("OrthoRayTracerCore Sphere IO Empty", emptyFrame)
        OrthoTestSupport.printFrame("OrthoRayTracerCore Sphere IO Populated", sphereFrame)

        assert(OrthoTestSupport.nonSpaceCount(emptyFrame) == 0)
        assert(OrthoTestSupport.nonSpaceCount(sphereFrame) > 0)
      }
  }
}

class OrthoRayTracerLightIoTest extends AnyFlatSpec with ChiselScalatestTester {
  "OrthoRayTracerCore light config" should "move the highlight when the light changes" in {
    test(new OrthoRayTracerCore(maxSpheres = 1, cxBits = 6, cyBits = 6, czBits = 5, rSqBits = 10, lightBits = 5)) {
      dut =>
        OrthoTestSupport.configureClockTimeout(dut.clock, maxSpheres = 1)

        val sphere = Seq(OrthoSphereInput(0, 0, 0, 196))

        OrthoTestSupport.pokeCoreConfig(dut, sphere, OrthoLightInput(4, 0))
        OrthoTestSupport.startCoreFrame(dut)
        val rightLitFrame = OrthoTestSupport.drainCoreFrame(dut)

        OrthoTestSupport.pokeCoreConfig(dut, sphere, OrthoLightInput(-4, 0))
        OrthoTestSupport.startCoreFrame(dut)
        val leftLitFrame = OrthoTestSupport.drainCoreFrame(dut)
        OrthoTestSupport.printFrame("OrthoRayTracerCore Light IO Right", rightLitFrame)
        OrthoTestSupport.printFrame("OrthoRayTracerCore Light IO Left", leftLitFrame)

        assert(
          OrthoTestSupport.patchBrightness(rightLitFrame, 44, 20, radius = 1) >
            OrthoTestSupport.patchBrightness(rightLitFrame, 36, 20, radius = 1)
        )
        assert(
          OrthoTestSupport.patchBrightness(leftLitFrame, 36, 20, radius = 1) >
            OrthoTestSupport.patchBrightness(leftLitFrame, 44, 20, radius = 1)
        )
      }
  }
}

class OrthoRayTracerMemTest extends AnyFlatSpec with ChiselScalatestTester {
  "OrthoRayTracerMem" should "load the scene from memory and render a non-empty frame" in {
    test(new OrthoRayTracerMem(
      maxSpheres = 1,
      cxBits = 6,
      cyBits = 6,
      czBits = 5,
      rSqBits = 10,
      lightBits = 5,
      useMemModel = true
    )) { dut =>
      OrthoTestSupport.configureClockTimeout(dut.clock, maxSpheres = 1)

      OrthoTestSupport.loadMemScene(
        dut,
        spheres = Seq(OrthoSphereInput(0, 0, 0, 196)),
        light = OrthoLightInput(4, 0),
        cxBits = 6,
        cyBits = 6,
        czBits = 5,
        lightBits = 5
      )

      OrthoTestSupport.startMemFrame(dut)
      val frame = OrthoTestSupport.drainMemFrame(dut)
      OrthoTestSupport.printFrame("OrthoRayTracerMem", frame)

      assert(OrthoTestSupport.nonSpaceCount(frame) > 0)
      assert(
        OrthoTestSupport.patchBrightness(frame, 44, 20, radius = 1) >
          OrthoTestSupport.patchBrightness(frame, 36, 20, radius = 1)
      )
    }
  }
}
