package org.fathens.colorworks.iccprofile

import java.io._
import _root_.org.fathens.colorworks.binarychain._

object ICC_Profile extends Builder[ICC_Profile] {
  def apply(input: InputStream) = {
    val ins = if (input.markSupported) input else new BufferedInputStream(input)
    val header = Header(ins)
    val tagTable = tag.TagTable(ins)
    val elements = new Elements(header.length + tagTable.length, ins).read(tagTable.sortedTags:_*)
    // ここで最終
    new ICC_Profile(header, tagTable, elements)
  }
  class Elements(lead: Long, ins: InputStream) {
    /**
     * TagRecordにこだわらず、offsetとsizeを持つものならば受け付けられるようにするために定義したタイプ
     */
    type OffsetRecord = {
      val offset: Long
      val size: Long
    }
    def read(records: OffsetRecord*) = build(Nil, records.toList)
    import tag._
    private[this] def build(right: List[TagElement], left: List[OffsetRecord]): List[TagElement] = left match {
      case Nil => right.reverse
      case t :: more => {
        val pos = right./:(lead) { (l, e) => l + e.length + e.commons.pad.length }
        val skip = t.offset - pos
        require(skip >= 0)
        val e = TagElement(ins, skip.toInt, t.size.toInt)
        build(e :: right, more)
      }
    }
  }
}

class ICC_Profile(val header: Header, val tagTable: tag.TagTable, val tagElements: List[tag.TagElement]) extends Expression {
  override val bytes = consists(header :: tagTable :: tagElements :_*)
  // 特定のタグを探す
  val tagPairs = tagTable.sortedTags zip tagElements
  def findTag(name: String) = for {
    pair <- tagPairs
    if (pair._1.tagName == name)
  } yield pair._2
}
