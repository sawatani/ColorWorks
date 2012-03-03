package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

object TagTable extends Builder[TagTable] {
  def apply(ins: InputStream) = {
    val tagCount = NumberU32(ins)
    val tags = TagRecord.times(ins, tagCount.value.toInt)
    new TagTable(tagCount, tags)
  }
}
class TagTable(val tagCount: NumberU32, val tags: ExpressionList[TagRecord]) extends Expression {
  val bytes = consists(tagCount, tags)

  val count = tagCount.value.toInt
  val sortedTags = tags.list sort { _.offset < _.offset }
}

object TagRecord extends BuilderFixed[TagRecord] {
  val length = TagSignature.length + NumberU32.length * 2
  def apply(ins: InputStream) = {
    new TagRecord(
      TagSignature(ins),
      NumberU32(ins),
      NumberU32(ins)
    )
  }
}
class TagRecord(val tagSignature: TagSignature, val offsetToElement: NumberU32, val sizeOfElement: NumberU32) extends Expression {
  val bytes = consists(tagSignature, offsetToElement, sizeOfElement)

  val tagName = tagSignature.name
  val offset = offsetToElement.value
  val size = sizeOfElement.value
}

/**
 * Tag signatures
 */
object TagSignature extends BuilderFixed[TagSignature] {
  val length = 4.toLong
  def apply(ins: InputStream) = {
    new TagSignature(read(ins, 4))
  }
  /**
   * 正式名称への対応
   */
  val table = Map(
    "A2B0" -> "AToB0Tag",
    "A2B1" -> "AToB1Tag",
    "A2B2" -> "AToB2Tag",
    "bXYZ" -> "blueMatrixColumnTag",
    "bTRC" -> "blueTRCTag",
    "B2A0" -> "BToA0Tag",
    "B2A1" -> "BToA1Tag",
    "B2A2" -> "BToA2Tag",
    "calt" -> "calibrationDateTimeTag",
    "targ" -> "charTargetTag",
    "chad" -> "chromaticAdaptationTag",
    "chrm" -> "chromaticityTag",
    "clro" -> "colorantOrderTag",
    "clrt" -> "colorantTableTag",
    "clot" -> "colorantTableOutTag",
    "cprt" -> "copyrightTag",
    "dmnd" -> "deviceMfgDescTag",
    "dmdd" -> "deviceModelDescTag",
    "gamt" -> "gamutTag",
    "kTRC" -> "grayTRCTag",
    "gXYZ" -> "greenMatrixColumnTag",
    "gTRC" -> "greenTRCTag",
    "lumi" -> "luminanceTag",
    "meas" -> "measurementTag",
    "bkpt" -> "mediaBlackPointTag",
    "wtpt" -> "mediaWhitePointTag",
    "ncl2" -> "namedColor2Tag",
    "resp" -> "outputResponseTag",
    "pre0" -> "preview0Tag",
    "pre1" -> "preview1Tag",
    "pre2" -> "preview2Tag",
    "desc" -> "profileDescriptionTag",
    "pseq" -> "profileSequenceDescTag",
    "rXYZ" -> "redMatrixColumnTag",
    "rTRC" -> "redTRCTag",
    "tech" -> "technologyTag",
    "vued" -> "viewingCondDescTag",
    "view" -> "viewingConditionsTag"
  )
}
class TagSignature(b: List[Byte]) extends String7bit(b) {
  require(b.length == TagSignature.length)
  val name = TagSignature.table.getOrElse(string, string)
}
