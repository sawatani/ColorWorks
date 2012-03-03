package org.fathens.colorworks.colorconvert

object ColorSpace {
  def apply(name: String): ColorSpace = {
    this apply Thread.currentThread.getContextClassLoader.getResource(name).openStream
  }
  def apply(ins: java.io.InputStream): ColorSpace = {
    this apply java.awt.color.ICC_Profile.getInstance(ins)
  }
  def apply(profile: java.awt.color.ICC_Profile): ColorSpace = {
    this apply new java.awt.color.ICC_ColorSpace(profile)
  }
  def apply(cs: java.awt.color.ICC_ColorSpace): ColorSpace = {
    new ColorSpace(cs)
  }
  def apply(color: java.awt.Color): ColorSpace = {
    this apply color.getColorSpace.asInstanceOf[java.awt.color.ICC_ColorSpace]
  }
  /**
   * プロファイル名と色の値を説明するための文字列にする
   */
  def makeDescription(color: java.awt.Color): String = {
    val profileName = color.getColorSpace match {
      case cs: java.awt.color.ICC_ColorSpace => ColorSpace(color).profileName
      case _ => "NotICC"
    }
    val c = color.getColorComponents(null).map(_.toDouble).toList
    presentingMinMax(color.getColorSpace).output(c, 1).zipWithIndex.map {
      case (v, i) => "%s=%d".format(color.getColorSpace.getName(i), v)
    }.mkString(profileName + "[", ",", "]")
  }
  private[ColorSpace] def presentingMinMax(cs: java.awt.color.ColorSpace) = {
    import java.awt.color.ColorSpace._
    cs.getType match {
      case TYPE_Lab => MinMax(List(0, -128, -128), List(100, 127, 127))
      case TYPE_RGB => MinMax(List(0, 0, 0), List(255, 255, 255))
      case TYPE_CMYK => MinMax(List(0, 0, 0, 0), List(100, 100, 100, 100))
    }
  }
  private[ColorSpace] case class MinMax(minValues: List[Double], maxValues: List[Double]) {
    def output(srcValues: List[Double], base: Int): List[Int] = {
      srcValues.zipWithIndex.map {
        case (v, i) => {
          val min = minValues(i)
          val max = maxValues(i)
          val diff = max - min
          v * diff / base + min
        }
      }.map(_.round.toInt).toList
    }
    def input(srcValues: List[Int], base: Int): List[Double] = {
      srcValues.map(_.toDouble).zipWithIndex.map {
        case (v, i) => {
          val min = minValues(i)
          val max = maxValues(i)
          val diff = max - min
          (v - min) * base / diff
        }
      }
    }
  }
}
class ColorSpace(val CS: java.awt.color.ICC_ColorSpace) {
  import ColorSpace._
  val numComponents = CS.getNumComponents
  val minmax = {
    val minValues = (0 until numComponents).map(CS.getMinValue(_).toDouble).toList
    val maxValues = (0 until numComponents).map(CS.getMaxValue(_).toDouble).toList
    MinMax(minValues, maxValues)
  }
  val profile = CS.getProfile
  lazy val profileName = {
    def strings(bytes: Array[Byte], list: List[String] = Nil): List[String] = {
      bytes.indexWhere(_ == 0) match {
        case -1 => list.filter(_.length > 0)
        case index => bytes.splitAt(index + 1) match {
          case (l, r) => strings(r, new String(l).trim :: list)
        }
      }
    }
    val data = profile.getData(java.awt.color.ICC_Profile.icSigProfileDescriptionTag)
    strings(data).head
  }
  /**
   * 色のインスタンスを生成する
   */
  def color(values: List[Double]): java.awt.Color = {
    require(values.length == numComponents)
    new java.awt.Color(CS, values.map(_.toFloat).toArray, 1f)
  }
  /**
   * 一般的に扱われる数値表現で色を生成する
   * <pre>
   * RGB: 0..255
   * CMYK: 0..100
   * </pre>
   */
  def makeColor(values: List[Int]): java.awt.Color = {
    val c = ColorSpace.presentingMinMax(CS).input(values, 0xffff)
    new java.awt.Color(CS, c.map(_.toFloat).toArray, 1f)
  }
  /**
   * 色の各コンポーネントの値を16bitで表現する
   */
  def toShort(color: java.awt.Color): List[Short] = {
    val c = color.getColorComponents(null).map(_.toDouble).toList
    presentingMinMax(CS).output(c, 0xffff).map(_.toShort)
  }
  /**
   * 16bitで表現された各コンポーネントで色を生成する
   */
  def fromShort(values: List[Short]): java.awt.Color = {
    val c = minmax.input(values.map(_ & 0xffff).map(_.toInt), 0xffff)
    new java.awt.Color(CS, c.map(_.toFloat).toArray, 1f)
  }
  /**
   * 指定されたレンダリングインテントで与えられた色をこの色空間に変換する
   */
  def convert(render: RenderingIntent.Value)(srcColor: java.awt.Color): java.awt.Color = convert(render, render)(srcColor)
  def convert(srcRender: RenderingIntent.Value, dstRender: RenderingIntent.Value)(srcColor: java.awt.Color): java.awt.Color = {
    val srcCS = ColorSpace(srcColor)
    val transform = {
      import sun.awt.color.ICC_Transform
      def tr(io: Int)(render: RenderingIntent.Value, cs: ColorSpace) = {
        try {
          new ICC_Transform(cs.profile, render.id, io)
        } catch {
          case ex: java.awt.color.CMMException => new ICC_Transform(cs.profile, RenderingIntent.Any.id, ICC_Transform.In)
        }
      }
      val t0 = tr(ICC_Transform.In)(srcRender, srcCS)
      val t1 = tr(ICC_Transform.Out)(dstRender, this)
      new ICC_Transform(Array(t0, t1))
    }
    val dst = {
      val src = srcCS.toShort(srcColor).toArray
      transform.colorConvert(src, null).toList
    }
    fromShort(dst)
  }
}
object RenderingIntent extends Enumeration {
  import java.awt.color.ICC_Profile._
  /**
   * 知覚的
   */
  val Perceptual = Value(icPerceptual)
  /**
   * 相対的
   */
  val RelativeColorimetric = Value(icRelativeColorimetric)
  /**
   * 彩度
   */
  val Saturation = Value(icSaturation)
  /**
   * 絶対的
   */
  val AbsoluteColorimetric = Value(icAbsoluteColorimetric)
  /**
   * どれでもいい
   */
  val Any = Value(-1)
}
