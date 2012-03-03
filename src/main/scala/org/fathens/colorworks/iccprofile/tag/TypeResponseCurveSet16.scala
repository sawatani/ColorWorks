package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

object TypeResponseCurveSet16 extends ElementBuilder[TypeResponseCurveSet16] {
  val typeSignature = "rcs2"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    val numChannels = NumberU16(ins)
    val count = NumberU16(ins)
    val offsets = NumberU32.times(ins, count.value)
    val curveBuilder = new CurveBuilder(numChannels.value)
    def readCurves(curves: List[Curve], fs: List[NumberU32]): ExpressionList[Curve] = fs match {
      case Nil => new ExpressionList(curves)
      case offset :: xs => {
        val pos = (commons :: numChannels :: count :: offsets.list ::: curves)./:(0){ _ + _.length.toInt }
        val padding = offset.value - pos
        readCurves(curveBuilder(ins, padding.toInt) :: curves, xs)
      }
    }
    val curves = readCurves(Nil, offsets.list sort { _.value < _.value })
    new TypeResponseCurveSet16(commons, numChannels, count, offsets, curves)
  }
  class CurveBuilder(numChannels: Int) extends Builder[Curve] {
    def apply(ins: InputStream) = apply(ins, 0)
    def apply(ins: InputStream, padding: Int) = {
      val pad = ReadBytes(ins, padding)
      val sig = String7bit(ins, 4)
      val setOfNumMeasurements = NumberU32.times(ins, numChannels)
      val pcsValues = setOfNumMeasurements.list map { numMeasurements =>
        XYZNumber.times(ins, numMeasurements.value.toInt)
      }
      val responses = setOfNumMeasurements.list map { numMeasurements =>
        Response16Number.times(ins, numMeasurements.value.toInt)
      }
      new Curve(pad, sig, setOfNumMeasurements, new ExpressionList(pcsValues), new ExpressionList(responses))
    }
  }
  class Curve(pad: Bytes,
              val signature: String7bit,
              val setOfNumMeasurements: ExpressionList[NumberU32],
              val pcsList: ExpressionList[ExpressionList[XYZNumber]],
              val responses: ExpressionList[ExpressionList[Response16Number]]) extends Expression {
    val bytes = consists(pad, signature, setOfNumMeasurements, pcsList, responses)
  }
}
class TypeResponseCurveSet16(commons: TagElement.CommonHeads,
                             val numChannels: NumberU16,
                             countTypes: NumberU16,
                             offsets: ExpressionList[NumberU32],
                             val curves: ExpressionList[TypeResponseCurveSet16.Curve]) extends TagElement(commons)
