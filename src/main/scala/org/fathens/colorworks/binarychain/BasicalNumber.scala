package org.fathens.colorworks.binarychain

import java.io._

/**
 * 数値の読み込みのための抽象Trait
 */
trait NumberExpression[I] extends Expression {
  def bigEdian(i: Int, j: Int): BigInt = {
    def calc(bs: List[Byte], v: BigInt): BigInt = bs match {
      case Nil => v
      case b :: left => {
        val a = BigInt(b & 0xff) << (left.length * 8)
        calc(left, v + a)
      }
    }
    calc(bytes.slice(i, j), BigInt(0))
  }
  val value: I
}

/*
 * 固定長の小数表現の読み込み
 */
trait NumberDecimalBuilder[T <: NumberDecimalExpression] extends BuilderFixed[T] {
  val decimal_denominator: Int
  def make(i: Int, n: Int, d: Int): Option[NumberDecimalExpression]
  def load(bs: Int*): T = apply(new ByteArrayInputStream((bs.map{_.toByte}).toArray))
}
trait NumberDecimalExpression extends NumberExpression[Double] {
  /**
   * 整数部を読み込んだ値
   */
  val integral: Int
  /**
   * 小数部の分子を読み込んだ値
   */
  val decimal_numerator: Int
  /**
   * 小数部の分母。これは各クラスで固定になる。
   */
  val decimal_denominator: Int
  /**
   * doubleでの値を計算した結果
   */
  lazy val value = integral + (decimal_numerator.toDouble / decimal_denominator)
  /**
   * otherに対して分母を合わせた結果を、タプルで返す。
   * _1 が自分自身の分母を変えた値。
   * _2 がotherの分母を変えた値。
   */
  def upto(other: NumberDecimalExpression): (NumberDecimalExpression, NumberDecimalExpression) = {
    // 最大公約数
    def gcd(n: Int, m: Int): Int = (n % m) match {
      case 0 => m
      case v => gcd(m, v)
    }
    // 最小公倍数
    def lcm(n: Int, m: Int) = n * m / gcd(n, m)
    // 共通分母
    val dd = lcm(this.decimal_denominator, other.decimal_denominator)
    def dn(d: NumberDecimalExpression) = d.decimal_numerator * (dd / d.decimal_denominator)
    (make(this.integral, dn(this), dd), make(other.integral, dn(other), dd))
  }
  /**
   * 各値から固定小数点表現を作り出す
   * @param i 整数部
   * @param n 分子部
   * @param d 分母部
   */
  def make(i: Int, n: Int, d: Int) = {
    val list = for {
      builder <- List(NumberS15Fixed16, NumberU16Fixed16, NumberU1Fixed15, NumberU8Fixed8)
      nd <- builder.make(i, n, d)
    } yield nd
    list match {
      case Nil => {
        val nn = n * (NumberS15Fixed16.decimal_denominator / d)
        NumberS15Fixed16.make(i, nn, NumberS15Fixed16.decimal_denominator).get
      }
      case v :: _ => v
    }
  }
}
/**
 * 符号あり整数部15bit/小数部16bit
 */
object NumberS15Fixed16 extends NumberDecimalBuilder[NumberS15Fixed16] {
  val length = 4.toLong
  val decimal_denominator = 0x10000
  def apply(ins: InputStream) = {
    new NumberS15Fixed16(read(ins, 4))
  }
  def make(i: Int, n: Int, d: Int) = {
    if (d == decimal_denominator && (-32768 until 32768).contains(i)) {
      val bi1 = i / 128 + (if (i < 0) 128 else 0)
      val bi2 = i % 256
      val bn1 = n / 256
      val bn2 = n % 256
      Some( load(bi1, bi2, bn1, bn2) )
    } else None
  }
}
class NumberS15Fixed16(val bytes: List[Byte]) extends NumberDecimalExpression {
  require(bytes.length == NumberS15Fixed16.length)
  val integral = {
    val v = bigEdian(0, 2).intValue
    if (v < 0x8000) v
    else (v - 0x8000) - 0x8000
  }
  val decimal_numerator = bigEdian(2, 4).intValue
  val decimal_denominator = NumberS15Fixed16.decimal_denominator
}
/**
 * 符号無し整数部16bit/小数部16bit
 */
object NumberU16Fixed16 extends NumberDecimalBuilder[NumberU16Fixed16] {
  val length = 4.toLong
  val decimal_denominator = 0x10000
  def apply(ins: InputStream) = {
    new NumberU16Fixed16(read(ins, 4))
  }
  def make(i: Int, n: Int, d: Int) = {
    if (d == decimal_denominator && (0 until 65536).contains(i)) {
      val bi1 = i / 128
      val bi2 = i % 256
      val bn1 = n / 256
      val bn2 = n % 256
      Some( load(bi1, bi2, bn1, bn2) )
    } else None
  }
}
class NumberU16Fixed16(val bytes: List[Byte]) extends NumberDecimalExpression {
  require(bytes.length == NumberU16Fixed16.length)
  val integral = bigEdian(0, 2).intValue
  val decimal_numerator = bigEdian(2, 4).intValue
  val decimal_denominator = NumberU16Fixed16.decimal_denominator
}
/**
 * 符号無し整数部1bit/小数部15bit
 */
object NumberU1Fixed15 extends NumberDecimalBuilder[NumberU1Fixed15] {
  val length = 2.toLong
  val decimal_denominator = 0x8000
  def apply(ins: InputStream) = {
    new NumberU1Fixed15(read(ins, 2))
  }
  def make(i: Int, n: Int, d: Int) = {
    if (d == decimal_denominator && (0 until 2).contains(i)) {
      val b1 = n / 256 + (i << 7)
      val b2 = n % 256
      Some( load(b1, b2) )
    } else None
  }
}
class NumberU1Fixed15(val bytes: List[Byte]) extends NumberDecimalExpression {
  require(bytes.length == NumberU1Fixed15.length)
  val integral = bigEdian(0, 1).intValue >> 7
  val decimal_numerator = bigEdian(0, 2).intValue & 0x7fff
  val decimal_denominator = NumberU1Fixed15.decimal_denominator
}
/**
 * 符号無し整数部8bit/小数部8bit
 */
object NumberU8Fixed8 extends NumberDecimalBuilder[NumberU8Fixed8] {
  val length = 2.toLong
  val decimal_denominator = 0x100
  def apply(ins: InputStream) = {
    new NumberU8Fixed8(read(ins, 2))
  }
  def make(i: Int, n: Int, d: Int) = {
    if (d == decimal_denominator && (0 until 256).contains(i)) {
      Some( load(i, n) )
    } else None
  }
}
class NumberU8Fixed8(val bytes: List[Byte]) extends NumberDecimalExpression {
  require(bytes.length == NumberU8Fixed8.length)
  val integral = bigEdian(0, 1).intValue
  val decimal_numerator = bigEdian(1, 2).intValue
  val decimal_denominator = NumberU8Fixed8.decimal_denominator
}

/*
 * 整数の読み込み
 */
trait NumberIntBuilder[T <: NumberIntExpression[_]] extends BuilderFixed[T]
trait NumberIntExpression[I] extends NumberExpression[I] {
  def bigEdian: BigInt = bigEdian(0, length.toInt)
}
/**
 * 符号なし 8-bit 整数
 */
object NumberU8 extends NumberIntBuilder[NumberU8] {
  val length = 1.toLong
  def apply(ins: InputStream) = {
    new NumberU8(read(ins, 1))
  }
}
class NumberU8(val bytes: List[Byte]) extends NumberIntExpression[Byte] {
  require(bytes.length == NumberU8.length)
  val value = bigEdian.byteValue
}
/**
 * 符号なし 16-bit 整数
 */
object NumberU16 extends NumberIntBuilder[NumberU16] {
  val length = 2.toLong
  def apply(ins: InputStream) = {
    new NumberU16(read(ins, 2))
  }
}
class NumberU16(val bytes: List[Byte]) extends NumberIntExpression[Int] {
  require(bytes.length == NumberU16.length)
  val value = bigEdian.intValue
}
/**
 * 符号なし 32 bit 整数
 */
object NumberU32 extends NumberIntBuilder[NumberU32] {
  val length = 4.toLong
  def apply(ins: InputStream) = {
    new NumberU32(read(ins, 4))
  }
}
class NumberU32(val bytes: List[Byte]) extends NumberIntExpression[Long] {
  require(bytes.length == NumberU32.length)
  val value = bigEdian.longValue
}
/**
 * 符号なし 64 bit 整数
 */
object NumberU64 extends NumberIntBuilder[NumberU64] {
  val length = 8.toLong
  def apply(ins: InputStream) = {
    new NumberU64(read(ins, 8))
  }
}
class NumberU64(val bytes: List[Byte]) extends NumberIntExpression[BigInt] {
  require(bytes.length == NumberU64.length)
  val value = bigEdian
}
