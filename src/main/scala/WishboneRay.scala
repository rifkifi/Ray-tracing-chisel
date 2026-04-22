import chisel3._
import chisel3.util._
import wishbone.WishboneIO

class WishboneRayTracer extends Module {
  val WB_ADDR_WIDTH = 32

  val wb = IO(Flipped(new WishboneIO(WB_ADDR_WIDTH)))

  val rayTracer = Module(new AsciiRayTracer(80, 25, 1, 1, 16))
  // fb removed entirely

  // Handshake
  val ackReg = RegInit(false.B)
  when(ackReg) {
    ackReg := false.B
  }.elsewhen(wb.cyc && wb.stb) {
    ackReg := true.B
  }

  wb.ack := ackReg
  val localAddr = wb.addr(7, 0)
  val doWrite = ackReg && wb.we
  val doRead  = ackReg && !wb.we

  // Camera registers
  val camPos = RegInit(0.U.asTypeOf(new Vector3(16, 8)))
  val camDir = RegInit(0.U.asTypeOf(new Vector3(16, 8)))
  val fovReg = RegInit(90.U(16.W))

  // Scene registers
  val sphereReg = RegInit(0.U.asTypeOf(new Sphere(16, 8)))
  val lightReg  = RegInit(0.U.asTypeOf(new Light(16, 8)))

  // Output / status registers
  val lastAsciiChar  = RegInit(0.U(8.W))
  val lastAsciiX     = RegInit(0.U(7.W))
  val lastAsciiY     = RegInit(0.U(6.W))
  val lastAsciiValid = RegInit(false.B)
  val frameDoneLatch = RegInit(false.B)

  // Start pulse
  val startPulse = WireDefault(false.B)

  // Connect ray tracer inputs
  rayTracer.io.cameraPos := camPos
  rayTracer.io.cameraDir := camDir
  rayTracer.io.fov := fovReg
  rayTracer.io.sphereData := sphereReg
  rayTracer.io.lightData := lightReg
  rayTracer.io.start := startPulse

  // Clear status when starting a new render
  when(startPulse) {
    frameDoneLatch := false.B
    lastAsciiValid := false.B
  }

  // Store latest ASCII output from ray tracer
  when(rayTracer.io.asciiValid) {
    lastAsciiChar  := rayTracer.io.asciiChar
    lastAsciiX     := rayTracer.io.x
    lastAsciiY     := rayTracer.io.y
    lastAsciiValid := true.B
  }

  when(rayTracer.io.frameComplete) {
    frameDoneLatch := true.B
  }

  // Register map writes
  when(doWrite) {
    switch(localAddr) {
      // 0x00 = control
      // bit 0 = start pulse
      // bit 1 = clear frameDoneLatch
      // bit 2 = clear lastAsciiValid
      is("h00".U) {
        when(wb.wrData(0)) { startPulse     := true.B }
        when(wb.wrData(1)) { frameDoneLatch := false.B }
        when(wb.wrData(2)) { lastAsciiValid := false.B }
      }

      // Camera position
      is("h08".U) { camPos.x.value := wb.wrData(15, 0) }
      is("h0C".U) { camPos.y.value := wb.wrData(15, 0) }
      is("h10".U) { camPos.z.value := wb.wrData(15, 0) }

      // Camera direction
      is("h14".U) { camDir.x.value := wb.wrData(15, 0) }
      is("h18".U) { camDir.y.value := wb.wrData(15, 0) }
      is("h1C".U) { camDir.z.value := wb.wrData(15, 0) }

      // FOV
      is("h20".U) { fovReg := wb.wrData(15, 0) }

      // Sphere center
      is("h30".U) { sphereReg.center.x.value := wb.wrData(15, 0) }
      is("h34".U) { sphereReg.center.y.value := wb.wrData(15, 0) }
      is("h38".U) { sphereReg.center.z.value := wb.wrData(15, 0) }

      // Sphere params
      is("h3C".U) { sphereReg.radius.value    := wb.wrData(15, 0) }
      is("h40".U) { sphereReg.radiusSq.value  := wb.wrData(15, 0) }
      is("h44".U) { sphereReg.specular.value  := wb.wrData(15, 0) }

      // Light position
      is("h50".U) { lightReg.position.x.value := wb.wrData(15, 0) }
      is("h54".U) { lightReg.position.y.value := wb.wrData(15, 0) }
      is("h58".U) { lightReg.position.z.value := wb.wrData(15, 0) }

      // Light intensity
      is("h5C".U) { lightReg.intensity.value  := wb.wrData(15, 0) }
    }
  }

  val rdData = WireDefault(0.U(32.W))

  switch(localAddr) {
    is("h00".U) { rdData := 0.U }

    // Status: bit 0 = busy, bit 1 = frameDone, bit 2 = asciiValid
    is("h04".U) {
      rdData := Cat(0.U(29.W), lastAsciiValid, frameDoneLatch, rayTracer.io.busy)
    }

    is("h08".U) { rdData := camPos.x.value }
    is("h0C".U) { rdData := camPos.y.value }
    is("h10".U) { rdData := camPos.z.value }

    is("h14".U) { rdData := camDir.x.value }
    is("h18".U) { rdData := camDir.y.value }
    is("h1C".U) { rdData := camDir.z.value }

    is("h20".U) { rdData := fovReg }

    // Latest pixel: [7:0]=char, [14:8]=x, [20:15]=y
    is("h24".U) {
      rdData := Cat(0.U(11.W), lastAsciiY, lastAsciiX, lastAsciiChar)
    }

    is("h30".U) { rdData := sphereReg.center.x.value }
    is("h34".U) { rdData := sphereReg.center.y.value }
    is("h38".U) { rdData := sphereReg.center.z.value }

    is("h3C".U) { rdData := sphereReg.radius.value }
    is("h40".U) { rdData := sphereReg.radiusSq.value }
    is("h44".U) { rdData := sphereReg.specular.value }

    is("h50".U) { rdData := lightReg.position.x.value }
    is("h54".U) { rdData := lightReg.position.y.value }
    is("h58".U) { rdData := lightReg.position.z.value }

    is("h5C".U) { rdData := lightReg.intensity.value }
  }

  wb.rdData := rdData
}