package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

private[tag] class SimpleArrayBuilder[E <: Expression](val typeSignature: String, builder: BuilderFixed[E]) extends ElementBuilder[TypeSimpleArray[E]] {
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    val numElements = length / builder.length
    val list = builder.times(ins, numElements.toInt)
    new TypeSimpleArray(commons, list)
  }
}
class TypeSimpleArray[E <: Expression](commons: TagElement.CommonHeads, val elements: ExpressionList[E]) extends TagElement(commons, elements) {
  def apply(index: Int) = elements(index)
}
object TypeUInt8Array extends SimpleArrayBuilder("ui08", NumberU8)
object TypeUInt16Array extends SimpleArrayBuilder("ui16", NumberU16)
object TypeUInt32Array extends SimpleArrayBuilder("ui32", NumberU32)
object TypeUInt64Array extends SimpleArrayBuilder("ui64", NumberU64)
object TypeU16Fixed16Array extends SimpleArrayBuilder("uf32", NumberU16Fixed16)
object TypeS15Fixed16Array extends SimpleArrayBuilder("sf32", NumberS15Fixed16)
object TypeXYZ extends SimpleArrayBuilder("XYZ ", XYZNumber)