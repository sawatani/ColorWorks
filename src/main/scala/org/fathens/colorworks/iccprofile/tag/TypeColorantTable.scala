package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

object TypeColorantTable extends ElementBuilder[TypeColorantTable] {
  val typeSignature = "clrt"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    val nc = NumberU32(ins)
    new TypeColorantTable(commons, nc, PCSColorant.times(ins, nc.value.toInt))
  }
  object PCSColorant extends Builder[PCSColorant] {
    def apply(ins: InputStream) = {
      new PCSColorant(
        String7bit(ins, 32),
        NumberU16(ins),
        NumberU16(ins),
        NumberU16(ins)
      )
    }
  }
  class PCSColorant(val name: String7bit, val value1: NumberU16, val value2: NumberU16, val value3: NumberU16) extends Expression {
    val bytes = consists(name, value1, value2, value3)
  }
}
class TypeColorantTable(commons: TagElement.CommonHeads,
                        val numColorants: NumberU32, val colorants: ExpressionList[TypeColorantTable.PCSColorant])
extends TagElement(commons, numColorants, colorants)
