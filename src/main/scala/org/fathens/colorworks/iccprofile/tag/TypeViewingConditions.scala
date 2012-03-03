package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

object TypeViewingConditions extends ElementBuilder[TypeViewingConditions] {
  val typeSignature = "view"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    new TypeViewingConditions(
      commons,
      XYZNumber(ins),
      XYZNumber(ins),
      NumberU32(ins)
    )
  }
}
class TypeViewingConditions(commons: TagElement.CommonHeads,
                            val illuminant: XYZNumber,
                            val surround: XYZNumber,
                            val illuminantType: NumberU32) extends TagElement(commons, illuminant, surround, illuminantType)
