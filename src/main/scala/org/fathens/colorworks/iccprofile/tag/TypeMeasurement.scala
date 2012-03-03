package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

object TypeMeasurement extends ElementBuilder[TypeMeasurement] {
  val typeSignature = "meas"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    new TypeMeasurement(
      commons,
      NumberU32(ins),
      XYZNumber(ins),
      NumberU32.times(ins, 3)
    )
  }
}
class TypeMeasurement(commons: TagElement.CommonHeads,
                      val observer: NumberU32,
                      val xyz: XYZNumber,
                      val measurements: ExpressionList[NumberU32]) extends TagElement(commons, observer, xyz, measurements)
