package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

object TypeSignature extends ElementBuilder[TypeSignature] {
  val typeSignature = "sig "
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    new TypeSignature(commons, String7bit(ins, length.toInt))
  }
}
class TypeSignature(commons: TagElement.CommonHeads, val signature: String7bit) extends TagElement(commons, signature) {
  val string = signature.string
}
