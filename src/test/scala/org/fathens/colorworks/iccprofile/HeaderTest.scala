package org.fathens.colorworks.iccprofile

import java.io._
import _root_.org.specs2.mutable._

class HeaderTest extends Specification {
  def dump(e: org.fathens.colorworks.binarychain.Expression) = println(e.bytes)
  def padding(len: Int) = (0 until len).map{ i => 0.toByte }.toList
  def newInputStream(is: Array[Int]) = new ByteArrayInputStream(is map { _.toByte })
  val AdobeRGB = Array(0x00, 0x00, 0x02, 0x30, // size
                       0x41, 0x44, 0x42, 0x45, // CMM Type
                       0x02, 0x10, 0x00, 0x00, // Version
                       0x6D, 0x6E, 0x74, 0x72, // Device Class
                       0x52, 0x47, 0x42, 0x20, // Color Space
                       0x58, 0x59, 0x5A, 0x20, // Profile Connection Space
                       0x07, 0xD0, 0x00, 0x08, 0x00, 0x0B, 0x00, 0x13, 0x00, 0x33, 0x00, 0x3B, // Timestamp
                       0x61, 0x63, 0x73, 0x70, // 'acsp' signature
                       0x41, 0x50, 0x50, 0x4C, // Platform
                       0x00, 0x00, 0x00, 0x00, // Flags for CMM
                       0x6E, 0x6F, 0x6E, 0x65, // Device Manufacturer
                       0x00, 0x00, 0x00, 0x00, // Device Model
                       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Device Attributes
                       0x00, 0x00, 0x00, 0x00, // Rendering Intent
                       0x00, 0x00, 0xF6, 0xD6, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0xD3, 0x2D, // XYZ of illuminant
                       0x41, 0x44, 0x42, 0x45, // Creator
                       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Profile ID
                       // Padding tail
                       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
  "AdobeRGBのヘッダ" in {
    val ins = newInputStream(AdobeRGB)
    val header = Header(ins)
    "サイズ" in {
      header.size.value must_== 0x230
    }
    "CMM Type" in {
      header.cmmType.string must_== "ADBE"
    }
    "Version" in {
      header.version.major must_== 2
      header.version.minior must_== 1
      header.version.bugfix must_== 0
      header.version.reserved.bytes must_== padding(2)
    }
    "Device Class" in {
      header.deviceClass.string must_== "mntr"
    }
    "Color Space" in {
      header.colorSpace.string must_== "RGB "
    }
    "Profile Connection Space" in {
      header.pcs.string must_== "XYZ "
    }
    "Timestamp" in {
      val offset = {
        import java.util.Calendar
        val cal = Calendar.getInstance
        (cal.get(Calendar.ZONE_OFFSET) + cal.get(Calendar.DST_OFFSET)) / (60 * 60 * 1000)
      }
      offset must_== +9
      val f = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
      f.format(header.timestamp.date) must_== "2000-08-12 04:51:59"
    }
    "Signature" in {
      header.signature.string must_== "acsp"
    }
    "Pratform" in {
      header.platform.string must_== "APPL"
    }
    "Flags for CMM" in {
      header.flags.isEmbedded must_== false
      header.flags.isCanWithoutEmbedded must_== true
    }
    "Device manufacturer" in {
      header.deviceManufacturer.string must_== "none"
    }
    "Device model" in {
      header.deviceModel.bytes must_== padding(4)
    }
    "Device attributes" in {
      header.deviceAttributes.isTransparency must_== false
      header.deviceAttributes.isNegative must_== false
      header.deviceAttributes.isMonotone must_== false
      header.deviceAttributes.isMatte must_== false
    }
    "Rendering Intent" in {
      header.renderingIntent.num.value must_== 0
      header.renderingIntent.isPerceptual must_== true
      header.renderingIntent.canonicalName must_== "Perceptual"
    }
    "Illuminant" in {
      header.illuminant.x.integral must_== 0x0000
      header.illuminant.x.decimal_numerator must_== 0xF6D6
      header.illuminant.y.integral must_== 0x0001
      header.illuminant.y.decimal_numerator must_== 0x0000
      header.illuminant.z.integral must_== 0x0000
      header.illuminant.z.decimal_numerator must_== 0xd32d
    }
    "Creator" in {
      header.creator.string must_== "ADBE"
    }
    "Profile ID" in {
      header.profileID.bytes must_== padding(16)
    }
    "Tail" in {
      header.reservedPadding.bytes must_== padding(28)
    }
  }
}
