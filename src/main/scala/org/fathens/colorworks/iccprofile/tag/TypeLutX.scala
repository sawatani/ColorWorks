package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._

/**
 * lut16TypeのBuilder。
 * lut8Typeとほぼ一緒のため大部分を共通化してある。
 */
object TypeLut16 extends lutX.LutXBuilder(NumberU16) {
  val typeSignature = "mft2"
  def numOfTables(ins: InputStream) = {
    val nn = intLoader.times(ins, 2)
    (nn, nn(0).value, nn(1).value)
  }
}
/**
 * lut8TypeのBuilder。
 * lut16Typeとほぼ一緒のため大部分を共通化してある。
 */
object TypeLut8 extends lutX.LutXBuilder(NumberU8) {
  val typeSignature = "mft1"
  def numOfTables(ins: InputStream) = (intLoader.times(ins, 0), 256, 256)
}
/**
 * lut16Typeとlut8Typeを表現するクラス。
 */
class TypeLutX[N <: NumberIntExpression[_]](commons: TagElement.CommonHeads, lc: lutX.LutChannels, nn: ExpressionList[N], tables: lutX.LutTables[N])
extends TagElement(commons, lc, nn, tables) {
  val numInputChannels = lc.nInput
  val numOutputChennels = lc.nOutput
  val numGridPoints = lc.nGrid
  val matrix = lc.matrix
  val inputTables = tables.inputTables
  val outputTables = tables.outputTables
  val clutValues = tables.clutValues
}

package lutX {
  /**
   * lut16Typeとlut8Typeとの共通抽象化クラス。
   * 整数を読み込むBuilerを引数にインスタンス化する。
   */
  abstract class LutXBuilder[N <: NumberIntExpression[_]](val intLoader: Builder[N]) extends ElementBuilder[TypeLutX[N]] {
    def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
      val lc = new LutChannels(
        NumberU8(ins),
        NumberU8(ins),
        NumberU8(ins),
        ReadBytes(ins, 1),
        Matrix3x3(ins)
      )
      val nn = numOfTables(ins)
      val tables = new LutTables(
        new TableBuilder(intLoader, lc.nInput).times(ins, nn._2),
        new CLUTBuilder(intLoader, lc.nGrid, lc.nInput, lc.nOutput)(ins),
        new TableBuilder(intLoader, lc.nOutput).times(ins, nn._3)
      )
      // インスタンス生成
      new TypeLutX(commons, lc, nn._1, tables)
      }
    /**
     * 入出力テーブルのそれぞれの数を示している部分を読み取り、その数を返す抽象メソッド。
     * 返り値はタプルで、それぞれ
     * _1 : テーブル数を読み込んだExpression（lut8Typeでは空リストになる）
     * _2 : 入力テーブル数（lut8Typeでは256固定）
     * _3 : 出力テーブル数（lut8Typeでは256固定）
     */
    def numOfTables(ins: InputStream): (ExpressionList[N], Int, Int)
  }
  /**
   * チャンネル数を格納している部分をまとめて読み込んでいるクラス。
   */
  class LutChannels(val numInputChannels: NumberU8, val numOutputChennels: NumberU8, val numGridPoints: NumberU8, val reserved: Bytes, val matrix: Matrix3x3) extends Expression {
    val bytes = consists(numInputChannels, numOutputChennels, numGridPoints, reserved, matrix)
    // チャンネル数
    val nInput = numInputChannels.value.toInt
    val nOutput = numOutputChennels.value.toInt
    val nGrid = numGridPoints.value.toInt
  }
  class LutTables[N <: NumberIntExpression[_]](val inputTables: ExpressionList[Table[N]], val clutValues: CLUTValues[N], val outputTables: ExpressionList[Table[N]]) extends Expression {
    val bytes = consists(inputTables, clutValues, outputTables)
  }
  /**
   * 入出力テーブル
   */
  class TableBuilder[N <: NumberIntExpression[_]](intLoader: Builder[N], val numInputChannels: Int) extends Builder[Table[N]] {
    def apply(ins: InputStream) = {
      new Table(intLoader.times(ins, numInputChannels))
    }
  }
  class Table[N <: NumberIntExpression[_]](val entries: ExpressionList[N]) extends ExpressionList(entries.list)
  /**
   * CLUT values
   */
  class CLUTBuilder[N <: NumberIntExpression[_]](intLoader: Builder[N], numGridPoints: Int, numInputChannels: Int, numOutputChannels: Int) extends Builder[CLUTValues[N]] {
    def apply(ins: InputStream) = {
      val n = numGridPoints ^ numInputChannels * numOutputChannels
      new CLUTValues(numGridPoints, numInputChannels, numOutputChannels, intLoader.times(ins, n))
    }
  }
  class CLUTValues[N <: NumberIntExpression[_]](val numGridPoint: Int, val numInputChannels: Int, val numOutputChannels: Int,
                   val values: ExpressionList[N]) extends Expression {
    val bytes = consists(values)
  }
}

/**
 * 3x3 のマトリックスを構成する。
 * <pre>
 *  | e00, e01, e02 |
 *  | e10, e11, e12 |
 *  | e20, e21, e22 |
 * </pre>
 */
object Matrix3x3 extends Builder[Matrix3x3] {
  def apply(ins: InputStream) = {
    new Matrix3x3(NumberS15Fixed16.times(ins, 9))
  }
}
class Matrix3x3(entries: ExpressionList[NumberS15Fixed16]) extends Expression {
  val bytes = consists(entries)
  // 各要素へのアクセス
  val e00 = entries(0)
  val e01 = entries(1)
  val e02 = entries(2)
  val e10 = entries(3)
  val e11 = entries(4)
  val e12 = entries(5)
  val e20 = entries(6)
  val e21 = entries(7)
  val e22 = entries(8)
}
