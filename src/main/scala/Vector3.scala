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
  
  def normalize(): Vector3 = { // Returns a normalized version of the vector.
    val len = sqrt(lengthSq) // Magnitude estimate used for normalization.
    val res = Wire(new Vector3(bitWidth, fracBits)) // Output wire for the normalized vector.
    when(len.value > 0.U) {
      res.x := this.x / len
      res.y := this.y / len
      res.z := this.z / len
    }.otherwise {
      res := this
    }
    res
  }
  
  private def sqrt(x: Fixed): Fixed = { // Approximates square root with Newton-Raphson iterations.
    val result = Wire(new Fixed(bitWidth, fracBits)) // Output wire for the square-root estimate.
    val iterations = 5 // Number of refinement iterations.
    val guess = Wire(Vec(iterations + 1, new Fixed(bitWidth, fracBits))) // Iterative guesses for the square root.
    
    guess(0) := Fixed.zero(bitWidth, fracBits)
    guess(0).value := x.value >> 1
    
    for (i <- 0 until iterations) {
      val div = Wire(new Fixed(bitWidth, fracBits)) // Division term x / guess(i).
      when(guess(i).value > 0.U) {
        div.value := (x.value << fracBits) / guess(i).value
      }.otherwise {
        div.value := x.value
      }
      
      val sum = Wire(new Fixed(bitWidth, fracBits)) // Sum used to average the current and reciprocal guesses.
      sum.value := guess(i).value + div.value
      
      guess(i + 1) := Fixed.zero(bitWidth, fracBits)
      guess(i + 1).value := sum.value >> 1
    }
    
    result := guess(iterations)
    result
  }
  
  override def toPrintable: Printable = { // Provides a readable debug representation.
    p"Vector3(x=${x.value}, y=${y.value}, z=${z.value})"
  }
}
