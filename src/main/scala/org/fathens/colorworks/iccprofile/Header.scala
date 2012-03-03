package org.fathens.colorworks.iccprofile

import java.io._
import _root_.org.fathens.colorworks.binarychain._

object Header extends BuilderFixed[Header] {
  val length = 128.toLong
  override def apply(ins: InputStream) = {
    def string4 = String7bit(ins, 4)
    new Header(
      NumberU32(ins),
      string4,
      ProfileVersion(ins),
      string4,
      string4,
      string4,
      DateTimeNumber(ins),
      string4,
      string4,
      ProfileFlags(ins),
      string4,
      string4,
      DeviceAttributes(ins),
      RenderingIntent(ins),
      XYZNumber(ins),
      string4,
      String7bit(ins, 16),
      ReadBytes(ins, 28)
    )
  }
}
class Header(
  val size: NumberU32,
  val cmmType: String7bit,
  val version: ProfileVersion,
  val deviceClass: String7bit,
  val colorSpace: String7bit,
  val pcs: String7bit,
  val timestamp: DateTimeNumber,
  val signature: String7bit,
  val platform: String7bit,
  val flags: ProfileFlags,
  val deviceManufacturer: String7bit,
  val deviceModel: String7bit,
  val deviceAttributes: DeviceAttributes,
  val renderingIntent: RenderingIntent,
  val illuminant: XYZNumber,
  val creator: String7bit,
  val profileID: String7bit,
  val reservedPadding: Bytes
) extends Expression {
  override val bytes = consists(size, cmmType, version, deviceClass, colorSpace, pcs, timestamp,
                                signature, platform,flags, deviceManufacturer, deviceModel, deviceAttributes,
                                renderingIntent, illuminant, creator, profileID, reservedPadding)
}

/**
 * profile version を格納する
 */
object ProfileVersion extends BuilderFixed[ProfileVersion] {
  val length = 4.toLong
  def apply(ins: InputStream) = {
    new ProfileVersion(
      ReadBytes(ins, 2),
      ReadBytes(ins, 2)
    )
  }
}
class ProfileVersion(vs: Bytes, val reserved: Bytes) extends Expression {
  val bytes = consists(vs, reserved)
  def read(index: Int) = vs.bytes(index) & 0xff
  val major = read(0)
  val minior = read(1) >> 4
  val bugfix = read(1) & 0x0f
}
/**
 * profile flags を格納する
 */
object ProfileFlags extends BuilderFixed[ProfileFlags] {
  val length = 4.toLong
  def apply(ins: InputStream) = {
    new ProfileFlags(read(ins, length.toInt))
  }
}
class ProfileFlags(val bytes: List[Byte]) extends Expression {
  val isEmbedded = (bytes(3) & 1) == 1
  val isCanWithoutEmbedded = (bytes(3) & 2) != 2
}
/**
 * device attributes を格納する
 */
object DeviceAttributes extends BuilderFixed[DeviceAttributes] {
  val length = 8.toLong
  def apply(ins: InputStream) = {
    new DeviceAttributes(read(ins, length.toInt))
  }
}
class DeviceAttributes(val bytes: List[Byte]) extends Expression {
  def testBit(i: Int) = ((bytes(3) & (1 << i)) >> i) == 1
  val isTransparency = testBit(0)
  val isMatte = testBit(1)
  val isNegative = testBit(2)
  val isMonotone = testBit(3)
}
/**
 * Rendering Intent を格納する
 */
object RenderingIntent extends BuilderFixed[RenderingIntent] {
  val length = NumberU16.length * 2
  def apply(ins: InputStream) = {
    new RenderingIntent(
      NumberU16(ins),
      NumberU16(ins)
    )
  }
}
class RenderingIntent(val reserved: NumberU16, val num: NumberU16) extends Expression {
  val bytes = consists(reserved, num)
  val isPerceptual = num.value == 0
  val isRelative = num.value == 1
  val isSaturation = num.value == 2
  val isAbsolute = num.value == 3
  val canonicalName = num.value match {
    case 0 => "Perceptual"
    case 1 => "Media-Relative Colorimetric"
    case 2 => "Saturation"
    case 3 => "ICC-Absolute Colorimetric"
  }
}