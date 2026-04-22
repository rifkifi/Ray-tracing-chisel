import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

//=============================================================================
// Custom Fixed-Point Implementation
//=============================================================================
class Fixed(val bitWidth: Int, val fracBits: Int) extends Bundle {
  val value = UInt(bitWidth.W)

  private def asSIntValue: SInt = value.asSInt
  
  // Convert from Double (for testing)
  def fromDouble(d: Double): UInt = {
    Fixed.rawBits(d, bitWidth, fracBits).U(bitWidth.W)
  }
  
  // Convert to Double (for simulation)
  def toDouble: Double = {
    Fixed.toDouble(this)
  }
  
  // Arithmetic operations
  def +(that: Fixed): Fixed = {
    val result = Wire(new Fixed(bitWidth, fracBits))
    result.value := (this.asSIntValue + that.asSIntValue).asUInt
    result
  }
  
  def -(that: Fixed): Fixed = {
    val result = Wire(new Fixed(bitWidth, fracBits))
    result.value := (this.asSIntValue - that.asSIntValue).asUInt
    result
  }
  
  def *(that: Fixed): Fixed = {
    val result = Wire(new Fixed(bitWidth, fracBits))
    val product = (this.asSIntValue * that.asSIntValue) >> fracBits
    result.value := product.asUInt
    result
  }
  
  def /(that: Fixed): Fixed = {
    val result = Wire(new Fixed(bitWidth, fracBits))
    val dividend = this.asSIntValue << fracBits
    result.value := (dividend / that.asSIntValue).asUInt
    result
  }
  
  def unary_- : Fixed = {
    val result = Wire(new Fixed(bitWidth, fracBits))
    result.value := (-this.value.asSInt).asUInt
    result
  }
  
  def >>(shift: Int): Fixed = {
    val result = Wire(new Fixed(bitWidth, fracBits))
    result.value := (this.asSIntValue >> shift).asUInt
    result
  }
  
  def <<(shift: Int): Fixed = {
    val result = Wire(new Fixed(bitWidth, fracBits))
    result.value := this.value << shift
    result
  }
  
  // Comparison operations
  def >(that: Fixed): Bool = this.asSIntValue > that.asSIntValue
  def <(that: Fixed): Bool = this.asSIntValue < that.asSIntValue
  def >=(that: Fixed): Bool = this.asSIntValue >= that.asSIntValue
  def <=(that: Fixed): Bool = this.asSIntValue <= that.asSIntValue
  def ===(that: Fixed): Bool = this.value === that.value
  
  def >(that: UInt): Bool = this.asSIntValue > that.asSInt
  def <(that: UInt): Bool = this.asSIntValue < that.asSInt
  
  // Assignment
  def :=(that: Fixed): Unit = {
    this.value := that.value
  }
  
  def :=(that: UInt): Unit = {
    this.value := that
  }
  
  // For debugging
  override def toPrintable: Printable = {
    Fixed.toPrintable(this)
  }
}

// Fixed-point companion object
object Fixed {
  def apply(bitWidth: Int, fracBits: Int): Fixed = new Fixed(bitWidth, fracBits)

  def toPrintable(fixed: Fixed, decimalDigits: Int = 4): Printable = {
    require(decimalDigits >= 0, "decimalDigits must be non-negative")

    val signedValue = fixed.value.asSInt
    val isNegative = signedValue < 0.S
    val absValue = Mux(isNegative, (-signedValue).asUInt, signedValue.asUInt)
    val signChar = Mux(isNegative, '-'.U(8.W), ' '.U(8.W))

    val integerPart = absValue >> fixed.fracBits
    val fracMask = ((BigInt(1) << fixed.fracBits) - 1).U(fixed.bitWidth.W)
    val fracRaw = absValue & fracMask
    val fracScale = BigInt(10).pow(decimalDigits)
    val fracScaled = (fracRaw * fracScale.U) >> fixed.fracBits

    Printables(Seq(
      Character(signChar),
      p"${integerPart}.${fracScaled}"
    ))
  }

  def toDouble(fixed: Fixed): Double = {
    require(
      fixed.value.isLit,
      s"Fixed.toDouble requires a literal value, got non-literal hardware for Q${fixed.bitWidth - fixed.fracBits}.${fixed.fracBits}"
    )
    val raw = fixed.value.litValue
    val signed =
      if (raw >= (BigInt(1) << (fixed.bitWidth - 1))) raw - (BigInt(1) << fixed.bitWidth)
      else raw
    signed.toDouble / (1 << fixed.fracBits)
  }

  private def encode(d: Double, bitWidth: Int, fracBits: Int): BigInt = {
    val scaled = BigDecimal(d) * BigDecimal(1 << fracBits)
    val raw = scaled.toBigInt
    val min = -(BigInt(1) << (bitWidth - 1))
    val max = (BigInt(1) << (bitWidth - 1)) - 1
    val clamped = raw.max(min).min(max)
    if (clamped < 0) clamped + (BigInt(1) << bitWidth) else clamped
  }

  def rawBits(d: Double, bitWidth: Int, fracBits: Int): BigInt = {
    encode(d, bitWidth, fracBits)
  }
  
  def fromDouble(d: Double, bitWidth: Int, fracBits: Int): Fixed = {
    (new Fixed(bitWidth, fracBits)).Lit(
      _.value -> encode(d, bitWidth, fracBits).U(bitWidth.W)
    )
  }
  
  def zero(bitWidth: Int, fracBits: Int): Fixed = {
    (new Fixed(bitWidth, fracBits)).Lit(_.value -> 0.U(bitWidth.W))
  }
  
  def one(bitWidth: Int, fracBits: Int): Fixed = {
    (new Fixed(bitWidth, fracBits)).Lit(_.value -> (1 << fracBits).U(bitWidth.W))
  }
  
  def half(bitWidth: Int, fracBits: Int): Fixed = {
    (new Fixed(bitWidth, fracBits)).Lit(_.value -> (1 << (fracBits - 1)).U(bitWidth.W))
  }
}
