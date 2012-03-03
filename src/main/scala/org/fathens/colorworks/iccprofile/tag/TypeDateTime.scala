package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

object TypeDateTime extends ElementBuilder[TypeDateTime] {
  val typeSignature = "dtim"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    new TypeDateTime(commons, DateTimeNumber(ins))
  }
}
class TypeDateTime(commons: TagElement.CommonHeads, val dateTime: DateTimeNumber)
extends TagElement(commons, dateTime)
