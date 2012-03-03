package org.fathens.colorworks.colorconvert

object CIELab {
  val minL = 0
  val maxL = 100
  val minA = -134
  val maxA = 220
  val minB = -140
  val maxB = 122
}
/**
 * 値域が以下のようになっている CIE 1976(L*a*b*) を表すクラス。
 * L:[0..100]
 * a:[-134..220]
 * b:[-140..122]
 */
case class CIELab(l: Double, a: Double, b: Double) {
  import CIELab._
  require(minL <= l & l <= maxL)
  require(minA <= a & a <= maxA)
  require(minB <= b & b <= maxB)
  override def toString: String = "%02.3f, %02.3f, %02.3f".format(l, a, b)
}
/**
 * 白色点を保持した CIE 1976(L*a*b*)
 */
object Lab {
  def getInstance(color: java.awt.Color): Lab = {
    XYZ.getInstance(color).toLab
  }
}
case class Lab(whitepoint: Illuminant, value: CIELab) {
  override def toString: String = "" + value + "(" + whitepoint + ")"
  /**
   * CIE XYZ に変換します。
   */
  def toXYZ: XYZ = {
    def fX = (value.a / 500) + fY
    def fY = (value.l + 16) / 116
    def fZ = fY - (value.b / 200)
    // 定数
    val e = 216.0 / 24389.0 // near to 0.008856
    val k = 24389.0 / 27.0 // near to 903.3
    // 条件分けされた式
    def xr = {
      val x3 = math.pow(fX, 3)
      if (x3 > e) {
        x3
      } else {
        (116 * fX - 16) / k
      }
    }
    def yr = {
      if (value.l > k * e) {
        math.pow((value.l + 16) / 116, 3)
      } else {
        value.l / k
      }
    }
    def zr = {
      val z3 = math.pow(fZ, 3)
      if (z3 > e) {
        z3
      } else {
        (116 * fZ - 16) / k
      }
    }
    XYZ(whitepoint, CIEXYZ(xr * whitepoint.x, yr * whitepoint.y, zr * whitepoint.z))
  }
  def toColor(cs: ColorSpace): java.awt.Color = {
    toXYZ.toColor(cs)
  }
}