package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

trait ElementBuilder[T <: TagElement] extends Builder[T] {
  def apply(ins: InputStream) = apply(ins, 0, Long.MaxValue)
  /**
   * padding分をスキップしその後の length の長さを読み込む。
   * 先頭の4bytesは type signature として読み込む。
   */
  def apply(ins: InputStream, padding: Int, length: Long): T = {
    val commons = TagElement.CommonHeads(ins, padding)
    build(commons, ins, length - commons.length)
  }
  /**
   * スキップ分を含む共通部分を読み込んだ後でその後の length 分を読み込む。
   */
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long): T
  /**
   * 実装している type signature
   */
  val typeSignature: String
}

object TagElement extends ElementBuilder[TagElement] {
  val typeSignature = null
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    new TagElement(commons, ReadBytes(ins, length.toInt))
  }
  /**
   * 全てのタイプのタグに共通の始まり部分
   */
  object CommonHeads extends BuilderFixed[CommonHeads] {
    val length = TagTypeSignature.length + 4
    def apply(ins: InputStream) = apply(ins, 0)
    def apply(ins: InputStream, padding: Int) = {
      new CommonHeads(
        ReadBytes(ins, padding),
        TagTypeSignature(ins),
        ReadBytes(ins, 4)
      )
    }
  }
  class CommonHeads(val pad: Bytes, val typeSignature: TagTypeSignature, val reserved: Bytes) extends Expression {
    val bytes = consists(typeSignature, reserved)
    require(length == CommonHeads.length)
  }
}
class TagElement(val commons: TagElement.CommonHeads, val parts: Expression*) extends Expression {
  val bytes = consists(commons :: parts.toList :_*)
  /**
   * type signature
   */
  val tagType = commons.typeSignature.name
  /**
   * tag type に従って変換する
   */
  def transformTo[A <: TagElement](builder: ElementBuilder[A]) = {
    if (commons.typeSignature.string != builder.typeSignature) None
    else Some( builder(newInputStream, 0, length) )
  }
}

/**
 * Tag signatures
 */
object TagTypeSignature extends BuilderFixed[TagTypeSignature] {
  val length = 4.toLong
  def apply(ins: InputStream) = {
    new TagTypeSignature(read(ins, 4))
  }
  /**
   * 正式名称への対応
   */
  val table = Map(
    "chrm" -> "chromaticityType",
    "clro" -> "colorantOrderType",
    "clrt" -> "colorantTableType",
    "curv" -> "curveType",
    "data" -> "dataType",
    "desc" -> "textDescriptionType",
    "dtim" -> "dateTimeType",
    "mft2" -> "lut16Type",
    "mft1" -> "lut8Type",
    "mAB " -> "lutAtoBType",
    "mBA " -> "lutBtoAType",
    "meas" -> "measurementType",
    "mluc" -> "multiLocalizedUnicodeType",
    "ncl2" -> "namedColor2Type",
    "para" -> "parametricCurveType",
    "pseq" -> "profileSequenceDescType",
    "rcs2" -> "responseCurveSet16Type",
    "sf32" -> "s15Fixed16ArrayType",
    "sig " -> "signatureType",
    "text" -> "textType",
    "uf32" -> "u16Fixed16ArrayType",
    "ui16" -> "uInt16ArrayType",
    "ui32" -> "uInt32ArrayType",
    "ui64" -> "uInt64ArrayType",
    "ui08" -> "uInt8ArrayType",
    "view" -> "viewingConditionsType",
    "XYZ " -> "XYZType"
  )
}
class TagTypeSignature(b: List[Byte]) extends String7bit(b) {
  require(length == TagTypeSignature.length)
  val name = TagTypeSignature.table.getOrElse(string, string)
}