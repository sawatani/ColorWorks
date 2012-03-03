package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._

object TypeCurve extends ElementBuilder[TypeCurve] {
  val typeSignature = "curv"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    val n = NumberU32(ins)
    new TypeCurve(commons, n, NumberU16.times(ins, n.value.toInt))
  }
}
class TypeCurve(commons: TagElement.CommonHeads,
                val numEntries: NumberU32, val entries: ExpressionList[NumberU16])
extends TagElement(commons, numEntries, entries) {
  val isStraight = numEntries.value == 0
  val gamma = if (numEntries.value == 1) Some( NumberU8Fixed8(entries.list(0).newInputStream) ) else None
}
