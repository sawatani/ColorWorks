package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

object TypeTextDescription extends ElementBuilder[TypeTextDescription] {
  val typeSignature = "desc"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    val len = NumberU32(ins)
    val text = String7bit(ins, len.value.toInt)
    new TypeTextDescription(
      commons,
      len,
      text,
      NumberU32(ins),
      NumberU32(ins),
      NumberU16(ins),
      NumberU8(ins),
      NumberU8.times(ins, 67)
    )
  }
}
class TypeTextDescription(commons: TagElement.CommonHeads, len: NumberU32, val text: String7bit, val unicodeCode: NumberU32, val unicodeCount: NumberU32, val scriptCode: NumberU16, val scriptCount: NumberU8, reserved: ExpressionList[NumberU8])
extends TagElement(commons, text, unicodeCode, unicodeCount, scriptCode, scriptCount, reserved) {
  val string = text.string
}
