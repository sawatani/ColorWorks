package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._

object TypeColorantOrder extends ElementBuilder[TypeColorantOrder] {
  val typeSignature = "clro"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    val nc = NumberU32(ins)
    val ncf = NumberU8(ins)
    new TypeColorantOrder(commons, nc, ncf, NumberU8.times(ins, nc.value.toInt))
  }
}
class TypeColorantOrder(commons: TagElement.CommonHeads,
                        val numColorants: NumberU32, val numColorantsFirst: NumberU8, val colorants: ExpressionList[NumberU8])
extends TagElement(commons, numColorants, numColorantsFirst, colorants)
