import chisel3._
import chisel3.util._

//=============================================================================
// Vector3 with Fixed-Point
//=============================================================================
class Vector3(val bitWidth: Int = 32, val fracBits: Int = 16) extends Bundle { // 3D fixed-point vector bundle configuration.
  val x = new Fixed(bitWidth, fracBits) // X component of the vector.
  val y = new Fixed(bitWidth, fracBits) // Y component of the vector.
  val z = new Fixed(bitWidth, fracBits) // Z component of the vector.
  
  def +(that: Vector3): Vector3 = { // Adds two vectors component-wise.
    val res = Wire(new Vector3(bitWidth, fracBits)) // Output wire for the summed vector.
    res.x := this.x + that.x
    res.y := this.y + that.y
    res.z := this.z + that.z
    res
  }
  
  def -(that: Vector3): Vector3 = { // Subtracts another vector component-wise.
    val res = Wire(new Vector3(bitWidth, fracBits)) // Output wire for the difference vector.
    res.x := this.x - that.x
    res.y := this.y - that.y
    res.z := this.z - that.z
    res
  }
  
  def *(scalar: Fixed): Vector3 = { // Scales the vector by a fixed-point scalar.
    val res = Wire(new Vector3(bitWidth, fracBits)) // Output wire for the scaled vector.
    res.x := this.x * scalar
    res.y := this.y * scalar
    res.z := this.z * scalar
    res
  }
  
  def unary_- : Vector3 = { // Negates each component of the vector.
    val res = Wire(new Vector3(bitWidth, fracBits)) // Output wire for the negated vector.
    res.x := -this.x
    res.y := -this.y
    res.z := -this.z
    res
  }
  
  def dot(that: Vector3): Fixed = { // Computes the dot product with another vector.
    val res = Wire(new Fixed(bitWidth, fracBits)) // Output wire for the dot-product result.
    val temp = (this.x * that.x) + (this.y * that.y) + (this.z * that.z) // Intermediate sum of multiplied components.
    res := temp
    res
  }
  
  def lengthSq: Fixed = dot(this) // Squared vector length.

  def maxNorm(): Fixed = maxComponentAbs() // Largest absolute component magnitude.

  def length(): Fixed = sqrt(lengthSq) // Euclidean vector length.
  
def normalize(): Vector3 = {
  val scale = maxNorm()
  val res = Wire(new Vector3(bitWidth, fracBits))
  when(scale.value > 0.U) {
    res.x.value := (this.x.value.asSInt >> fracBits).asUInt
    res.y.value := (this.y.value.asSInt >> fracBits).asUInt
    res.z.value := (this.z.value.asSInt >> fracBits).asUInt
  }.otherwise {
    res := this
  }
  res
}

  private def absFixed(value: Fixed): Fixed = { // Returns the absolute value of a fixed-point number.
    val res = Wire(new Fixed(bitWidth, fracBits))
    res.value := Mux(value.value.asSInt < 0.S, (-value.value.asSInt).asUInt, value.value)
    res
  }

  private def maxFixed(a: Fixed, b: Fixed): Fixed = Mux(a > b, a, b) // Returns the larger of two fixed-point values.

  private def maxComponentAbs(): Fixed = { // Returns the largest absolute component magnitude.
    val absX = absFixed(this.x)
    val absY = absFixed(this.y)
    val absZ = absFixed(this.z)
    maxFixed(absX, maxFixed(absY, absZ))
  }
  
 private def sqrt(x: Fixed): Fixed = {
  val n = bitWidth  // 16
  val result = Wire(new Fixed(bitWidth, fracBits))
  
  // We work with 2*bitWidth bits of precision internally
  // rem = running remainder, root = partial root built up
  val rem  = Wire(Vec(n/2 + 1, UInt((n+2).W)))
  val root = Wire(Vec(n/2 + 1, UInt((n+2).W)))
  
  rem(0)  := x.value  // start with the input
  root(0) := 0.U
  
  for (i <- 0 until n/2) {
    // Bit position we're computing, from MSB down
    val bitPos = (n/2 - 1 - i).U
    
    // Trial: Y = 2*root + 2^(2*bitPos) ... simplified for binary:
    // candidate = root with this bit set, shifted appropriately
    val candidate = (root(i) << 1) | (1.U << (bitPos * 2.U))
    
    // Does candidate fit in the remainder?
    when(candidate <= rem(i)) {
      rem(i+1)  := rem(i) - candidate
      root(i+1) := root(i) | (1.U << bitPos)
    }.otherwise {
      rem(i+1)  := rem(i)
      root(i+1) := root(i)
    }
  }
  
  result.value := root(n/2)(bitWidth-1, 0)
  result
}
  
  override def toPrintable: Printable = { // Provides a readable debug representation.
    p"Vector3(x=${x.value}, y=${y.value}, z=${z.value})"
  }
}
