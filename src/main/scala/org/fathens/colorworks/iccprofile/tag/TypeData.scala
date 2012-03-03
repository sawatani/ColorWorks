package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

object TypeData extends ElementBuilder[TypeData] {
  val typeSignature = "data"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    val flag = new DataFlag(read(ins, 4))
    new TypeData(commons, flag, String7bit(ins, (length - flag.length).toInt))
  }
  /**
   * Flag for data type
   */
  class DataFlag(b: List[Byte]) extends NumberU32(b) {
    val isString = value == 0
    val isBinary = value == 1
  }
}
class TypeData(commons: TagElement.CommonHeads, val flag: TypeData.DataFlag, val data: String7bit)
extends TagElement(commons, flag, data) {
  def asString = data.string
  def asBinary = data.bytes
}
