package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._

object TypeLutA2B extends lutAB.LutBuilder {
  val typeSignature = "mAB "
}
object TypeLutB2A extends lutAB.LutBuilder {
  val typeSignature = "mBA "
}
class TypeLutAB(commons: TagElement.CommonHeads, begining: lutAB.Begining, curves: lutAB.CurveData) extends TagElement(commons, begining, curves)

package lutAB {
  abstract class LutBuilder extends ElementBuilder[TypeLutAB] {
    def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
      val b = Begining(ins)
      val c = new CurveDataBuilder(b, length - b.length)(ins)
      new TypeLutAB(commons, b, c)
    }
  }
  /**
   * lutAtoBTypeとlutBtoATypeに共通する始まり部分を表す。
   */
  object Begining extends Builder[Begining] {
    def apply(ins: InputStream) = {
      new Begining(
        NumberU8(ins),
        NumberU8(ins),
        ReadBytes(ins, 1),
        NumberU32(ins),
        NumberU32(ins),
        NumberU32(ins),
        NumberU32(ins),
        NumberU32(ins)
      )
    }
  }
  class Begining(val numInputChannels: NumberU8, val numOutputChannels: NumberU8, val reserved: Bytes,
                 val offsetToB: NumberU32,
                 val offsetToMatrix: NumberU32,
                 val offsetToM: NumberU32,
                 val offsetToCLUT: NumberU32,
                 val offsetToA: NumberU32)
  extends Expression {
    val bytes = consists(numInputChannels, numOutputChannels, reserved, offsetToB, offsetToMatrix, offsetToM, offsetToCLUT, offsetToA)
    val offsets = List(
      ("B", offsetToB.value),
      ("Matrix", offsetToMatrix.value),
      ("M", offsetToM.value),
      ("CLUT", offsetToCLUT.value),
      ("A", offsetToA.value)
    ) sort { _._2 < _._2 }
  }
  /**
   * カーブを納めている領域を表す
   */
  private[lutAB] class CurveDataBuilder(begining: Begining, length: Long) extends Builder[CurveData] {
    def apply(ins: InputStream) = {
      new CurveData(read(ins, length.toInt))
    }
  }
  class CurveData(val bytes: List[Byte]) extends Expression
}
