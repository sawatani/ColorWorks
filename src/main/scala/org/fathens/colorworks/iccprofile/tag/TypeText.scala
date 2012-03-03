package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

object TypeText extends ElementBuilder[TypeText] {
  val typeSignature = "text"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    new TypeText(
      commons,
      String7bit(ins, length.toInt)
    )
  }
}
class TypeText(commons: TagElement.CommonHeads, val text: String7bit) extends TagElement(commons, text) {
  val string = text.string
}
