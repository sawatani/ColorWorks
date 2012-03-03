package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

object TypeNamedColor2 extends ElementBuilder[TypeNamedColor2] {
  val typeSignature = "ncl2"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    val flag = NumberU32(ins)
    val numColors = NumberU32(ins)
    val numDevices = NumberU32(ins)
    val prefix = String7bit(ins, 32)
    val surfix = String7bit(ins, 32)
    val builder = new BuilderFixed[NamedColor] {
      val length = numDevices.value * 2 + 38
      def apply(ins: InputStream) = {
        new NamedColor(
          String7bit(ins, 32),
          NumberU16.times(ins, 3),
          NumberU16.times(ins, numDevices.value.toInt)
        )
      }
    }
    new TypeNamedColor2(commons, flag, numColors, numDevices, prefix, surfix, builder.times(ins, numColors.value.toInt))
  }
  class NamedColor(val rootName: String7bit, val pcsList: ExpressionList[NumberU16], val devices: ExpressionList[NumberU16]) extends Expression {
    val bytes = consists(rootName, pcsList, devices)
  }
}
class TypeNamedColor2(commons: TagElement.CommonHeads,
                      val flag: NumberU32,
                      val numColors: NumberU32,
                      val numDevices: NumberU32,
                      val prefix: String7bit,
                      val surfix: String7bit,
                      val colors: ExpressionList[TypeNamedColor2.NamedColor]
) extends TagElement(commons, flag, numColors, numDevices, prefix, surfix)
