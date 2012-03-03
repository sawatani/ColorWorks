package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._

object TypeParametricCurve extends ElementBuilder[TypeParametricCurve] {
  val typeSignature = "para"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    val funcType = NumberU16(ins)
    val reserved = ReadBytes(ins, 2)
    val params = funcType.value match {
      case 0 => NumberS15Fixed16.times(ins, 1)
      case 1 => NumberS15Fixed16.times(ins, 3)
      case 2 => NumberS15Fixed16.times(ins, 4)
      case 3 => NumberS15Fixed16.times(ins, 5)
      case 4 => NumberS15Fixed16.times(ins, 7)
    }
    new TypeParametricCurve(commons, funcType, reserved, params)
  }
}
class TypeParametricCurve(commons: TagElement.CommonHeads,
                          val funcType: NumberU16,
                          reserved: Bytes,
                          val parameters: ExpressionList[NumberS15Fixed16]) extends TagElement(commons, funcType, reserved, parameters) {
  val params = parameters.list map { _.value }
}
