package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._

object TypeChromaticity extends ElementBuilder[TypeChromaticity] {
  val typeSignature = "chrm"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    val nc = NumberU16(ins)
    val pp = Phosphor(ins)
    val cs = CoodinateXY.times(ins, nc.value)
    new TypeChromaticity(commons, nc, pp, cs)
  }
  object Phosphor extends Builder[Phosphor] {
    def apply(ins: InputStream) = {
      new Phosphor(read(ins, 2))
    }
    val table = Map(
      0 -> ("unknown", List[CoodinateXY]()),
      1 -> ("ITU-R BT.709", List[CoodinateXY](
      )),
      2 -> ("SMPTE RP145-1994", List[CoodinateXY](
      )),
      3 -> ("EBU Tech.3213-E", List[CoodinateXY](
      )),
      4 -> ("P22", List[CoodinateXY](
      ))
    )
  }
  class Phosphor(val bytes: List[Byte]) extends Expression {
    val value = bytes(0) << 8 + bytes(1)
    // 規定の値
    val name = Phosphor.table(value)._1
    val chromaticities = Phosphor.table(value)._2
  }
  object CoodinateXY extends Builder[CoodinateXY] {
    def apply(ins: InputStream) = {
      new CoodinateXY(NumberU16Fixed16(ins), NumberU16Fixed16(ins))
    }
  }
  class CoodinateXY(val x: NumberU16Fixed16, val y: NumberU16Fixed16) extends Expression {
    val bytes = consists(x, y)
  }
}
class TypeChromaticity(commons: TagElement.CommonHeads,
                       val numChannels: NumberU16, val phosphor: TypeChromaticity.Phosphor, xy: ExpressionList[TypeChromaticity.CoodinateXY])
extends TagElement(commons, numChannels, phosphor, xy)
