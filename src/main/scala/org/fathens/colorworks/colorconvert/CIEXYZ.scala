package org.fathens.colorworks.colorconvert

/**
 * 一般的な 0〜100付近 までを値域とするXYZ
 */
object CIEXYZ {
  val minX = 0
  val maxX = 199.99695
  val minY = 0
  val maxY = 199.99695
  val minZ = 0
  val maxZ = 199.99695
}
case class CIEXYZ(x: Double, y: Double, z: Double) {
  def this(src: NominalXYZ) = this(src.x * 100, src.y * 100, src.z * 100)
  import CIEXYZ._
  require(minX <= x)
  require(minY <= y)
  require(minZ <= z)
  override def toString: String = "%02.3f, %02.3f, %02.3f".format(x, y, z)
}
/**
 * 白色点を保持したXYZの値
 */
object XYZ {
  def getColorSpace: ColorSpace = {
    val CS = {
      import java.awt.color._
      ColorSpace.getInstance(ColorSpace.CS_CIEXYZ).asInstanceOf[ICC_ColorSpace]
    }
    ColorSpace(CS)
  }
  def getInstance(color: java.awt.Color, render: RenderingIntent.Value = RenderingIntent.Perceptual): XYZ = {
    val xyz = {
      val dst = XYZ.getColorSpace.convert(render, RenderingIntent.Any)(color)
      dst.getColorComponents(null)
    }
    val nXYZ = NominalXYZ(xyz(0), xyz(1), xyz(2))
    XYZ(Illuminant.D50, new CIEXYZ(nXYZ))
  }
}
case class XYZ(whitepoint: Illuminant, value: CIEXYZ) {
  override def toString: String = "" + value + "(" + whitepoint + ")"
  /**
   * 白色点を変換します。
   */
  def convertWhitepoint(dstWhitepoint: Illuminant): XYZ = {
    val vX = value.x * whitepoint.x / dstWhitepoint.x
    val vY = value.y * whitepoint.y / dstWhitepoint.y
    val vZ = value.z * whitepoint.z / dstWhitepoint.z
    XYZ(dstWhitepoint, CIEXYZ(vX, vY, vZ))
  }
  /**
   * CIE 1976(L*a*b*) に変換します。
   */
  def toLab: Lab = {
    def calc(s: Double, w: Double) = {
      val v = s / w
      if (v > (216.0 / 24389)) {
        math.cbrt(v)
      } else {
        (v * 24389 / 27 + 16) / 116
      }
    }
    val pX = calc(value.x, whitepoint.x)
    val pY = calc(value.y, whitepoint.y)
    val pZ = calc(value.z, whitepoint.z)
    Lab(whitepoint, CIELab((116 * pY - 16).round, (500 * (pX - pY)).round, (200 * (pY - pZ)).round))
  }
  def toColor(cs: ColorSpace, render: RenderingIntent.Value = RenderingIntent.Perceptual): java.awt.Color = {
    val srcColor = {
      val values = new NominalXYZ(value).toArray.map(_.toFloat)
      new java.awt.Color(XYZ.getColorSpace.CS, values, 1f)
    }
    cs.convert(RenderingIntent.Any, render)(srcColor)
  }
}
/**
 * 値域を 0〜1付近 までとしたXYZ
 */
object NominalXYZ {
  val minX = 0
  val maxX = 1.9999695
  val minY = 0
  val maxY = 1.9999695
  val minZ = 0
  val maxZ = 1.9999695
}
case class NominalXYZ(x: Double, y: Double, z: Double) {
  def this(src: CIEXYZ) = this(src.x / 100, src.y / 100, src.z / 100)
  import NominalXYZ._
  require(minX <= x)
  require(minY <= y)
  require(minZ <= z)
  override def toString: String = "%02.3f, %02.3f, %02.3f".format(x, y, z)
  def toArray: Array[Double] = Array(x, y, z)
}