package org.fathens.colorworks.iccprofile

import java.io._
import _root_.org.specs2.mutable._
import tag._

class ICC_Profile_ElementsTest extends Specification {
  case class osr(offset: Long, size: Long)
  implicit def charToInt(c: Char) = c.toInt
  def ins(ints: Int*) = new ByteArrayInputStream((ints map { _.toByte }).toArray)
  "leadが0バイトでも読み込む" >> {
    val reader = new ICC_Profile.Elements(0, ins('a', 'b', 'c', 'd', 0, 0, 0, 0, 1, 2, 3, 4))
    val es = reader.read(osr(0, 12))
    es.length must_== 1
    es(0).tagType must_== "abcd"
    es(0).length must_== 12
    es(0).commons.pad.length must_== 0
  }
  "leadが3バイトでも頭から読み込む。このときoffsetは3からスタートになる。" >> {
    val reader = new ICC_Profile.Elements(3, ins('a', 'b', 'c', 'd', 0, 0, 0, 0, 1, 2, 3, 4))
    val es = reader.read(osr(3, 11))
    es.length must_== 1
    es(0).tagType must_== "abcd"
    es(0).length must_== 11
    es(0).commons.pad.length must_== 0
  }
  "offsetが指定されていればleadより先ならpadでスキップする。一つ前までとoffsetが同じならスキップしない。" >> {
    val reader = new ICC_Profile.Elements(3,
                                          ins(0, 0,
                                              'a', 'b', 'c', 'd', 0, 0, 0, 0, 1, 2, 3, 4,
                                              'e', 'f', 'g', 'h', 0, 0, 0, 0, 2, 3, 4, 5
                                          ))
    val es = reader.read(osr(5, 12),
                         osr(17, 12))
    es.length must_== 2
    es(0).tagType must_== "abcd"
    es(0).length must_== 12
    es(0).commons.pad.length must_== 2
    es(1).tagType must_== "efgh"
    es(0).length must_== 12
    es(1).commons.pad.length must_== 0
  }
  "一つ前までの長さからoffsetまでスキップする" >> {
    val reader = new ICC_Profile.Elements(3,
                                          ins(0, 0,
                                              'a', 'b', 'c', 'd', 0, 0, 0, 0, 1, 2, 3, 4,
                                              0, 0, 0, 0, 0,
                                              'e', 'f', 'g', 'h', 0, 0, 0, 0, 2, 3, 4, 5
                                          ))
    val es = reader.read(osr(5, 12),
                         osr(22, 12))
    es.length must_== 2
    es(0).tagType must_== "abcd"
    es(0).length must_== 12
    es(0).commons.pad.length must_== 2
    es(1).tagType must_== "efgh"
    es(0).length must_== 12
    es(1).commons.pad.length must_== 5
  }
  "一つ前までのpadの長さも含めて合計する" >> {
    val reader = new ICC_Profile.Elements(3,
                                          ins(0, 0,
                                              'a', 'b', 'c', 'd', 0, 0, 0, 0, 1, 2, 3, 4,
                                              0, 0, 0, 0, 0,
                                              'e', 'f', 'g', 'h', 0, 0, 0, 0, 2, 3, 4, 5, 6, 7,
                                              0,
                                              'i', 'j', 'k', 'l', 0, 0, 0, 0, 3, 4
                                          ))
    val es = reader.read(osr(5, 12),
                         osr(22, 14),
                         osr(37, 10))
    es.length must_== 3
    es(0).tagType must_== "abcd"
    es(0).length must_== 12
    es(0).commons.pad.length must_== 2
    es(1).tagType must_== "efgh"
    es(1).length must_== 14
    es(1).commons.pad.length must_== 5
    es(2).tagType must_== "ijkl"
    es(2).length must_== 10
    es(2).commons.pad.length must_== 1
  }
  "offsetが一つ前までの長さよりも手前ならエラーになる" >> {
    val reader = new ICC_Profile.Elements(3,
                                          ins(0, 0,
                                              'a', 'b', 'c', 'd', 0, 0, 0, 0, 1, 2, 3, 4,
                                              0, 0, 0, 0, 0,
                                              'e', 'f', 'g', 'h', 0, 0, 0, 0, 2, 3, 4, 5,
                                              'i', 'j', 'k', 'l', 0, 0, 0, 0, 3, 4, 5, 6
                                          ))
    reader.read(osr(5, 12), osr(22, 12), osr(33, 12)) must throwAn[IllegalArgumentException]
  }
  "変換する時はpadの長さは無視する" >> {
    val reader = new ICC_Profile.Elements(128, ins(0, 0, 0, 't', 'e', 'x', 't', 0, 0, 0, 0, 'k', 'u', 'n', 'i', 'o'))
    val es = reader.read(osr(131, 13))
    es.length must_== 1
    es(0).tagType must_== "textType"
    es(0).length must_== 13
    es(0).commons.pad.length must_== 3
    val t = (es(0) transformTo TypeText).get
    t.length must_== 13
    t.string must_== "kunio"
    t.commons.pad.length must_== 0
  }
}
class ICC_ProfileTest extends Specification {
  implicit def hexString(e: org.fathens.colorworks.binarychain.Expression) = new {
    def toHexString = {
      e.bytes./:(new StringBuilder) { (s, b) =>
        val hex = {
          val u = "0" + ((b & 0x00ff).toHexString.toUpperCase)
          val h = u.substring(u.length - 2)
          if ((s.length + 4) % (3 * 4 + 1) == 0) h + ", "
          else h + " "
        }
        s append hex
      }.toString
    }
  }
  "AdobeRGBのタグを全て読み込める" >> {
    val url = getClass.getClassLoader.getResource("AdobeRGB1998.icc")
    val profile = ICC_Profile(url.openStream)
    List(
      ("copyrightTag", "textType"),
      ("profileDescriptionTag", "textDescriptionType"),
      ("mediaWhitePointTag", "XYZType"),
      ("mediaBlackPointTag", "XYZType"),
      ("redTRCTag", "curveType"),
      ("greenTRCTag", "curveType"),
      ("blueTRCTag", "curveType"),
      ("redMatrixColumnTag", "XYZType"),
      ("greenMatrixColumnTag", "XYZType"),
      ("blueMatrixColumnTag", "XYZType")
    ) zip profile.tagPairs foreach { p =>
      p._2._1.tagName must_== p._1._1
      p._2._2.tagType must_== p._1._2
    }
    import tag._
    def readTag[T <: TagElement](name: String, builder: ElementBuilder[T]) = {
      val found = (profile.findTag(name))(0)
      println("Found tag:'" + name + "'=" + found.toHexString + "(length=" + found.length + "), converting to " + builder.typeSignature)
      found.transformTo(builder).get
    }
    "copyrightTag" >> {
      val copyrightTag = readTag("copyrightTag", TypeText)
      copyrightTag.string must_== "Copyright 2000 Adobe Systems Incorporated"
    }
    "profileDescriptionTag" >> {
      val profileDescriptionTag = readTag("profileDescriptionTag", TypeTextDescription)
      profileDescriptionTag.string must_== "Adobe RGB (1998)"
    }
    "mediaWhitePointTag" >> {
      val mediaWhitePointTag = readTag("mediaWhitePointTag", TypeXYZ)
      mediaWhitePointTag.elements.list.length must_== 1
      mediaWhitePointTag(0).valueX must_== 0.9504547119140625
      mediaWhitePointTag(0).valueY must_== 1
      mediaWhitePointTag(0).valueZ must_== 1.08905029296875
    }
    "mediaBlackPointTag" >> {
      val mediaBlackPointTag = readTag("mediaBlackPointTag", TypeXYZ)
      mediaBlackPointTag.elements.list.length must_== 1
      mediaBlackPointTag(0).valueX must_== 0
      mediaBlackPointTag(0).valueY must_== 0
      mediaBlackPointTag(0).valueZ must_== 0
    }
    "redTRCTag" >> {
      val redTRCTag = readTag("redTRCTag", TypeCurve)
      redTRCTag.numEntries.value must_== 1
      redTRCTag.isStraight must_== false
      redTRCTag.gamma.get.value must_== 2.19921875
    }
    "greenTRCTag" >> {
      val greenTRCTag = readTag("greenTRCTag", TypeCurve)
      greenTRCTag.numEntries.value must_== 1
      greenTRCTag.isStraight must_== false
      greenTRCTag.gamma.get.value must_== 2.19921875
    }
    "blueTRCTag" >> {
      val blueTRCTag = readTag("blueTRCTag", TypeCurve)
      blueTRCTag.numEntries.value must_== 1
      blueTRCTag.isStraight must_== false
      blueTRCTag.gamma.get.value must_== 2.19921875
    }
    "redMatrixColumnTag" >> {
      val redMatrixColumnTag = readTag("redMatrixColumnTag", TypeXYZ)
      redMatrixColumnTag.elements.list.length must_== 1
      redMatrixColumnTag(0).valueX must_== 0.6097412109375
      redMatrixColumnTag(0).valueY must_== 0.3111114501953125
      redMatrixColumnTag(0).valueZ must_== 0.01947021484375
    }
    "greenMatrixColumnTag" >> {
      val greenMatrixColumnTag = readTag("greenMatrixColumnTag", TypeXYZ)
      greenMatrixColumnTag.elements.list.length must_== 1
      greenMatrixColumnTag(0).valueX must_== 0.2052764892578125
      greenMatrixColumnTag(0).valueY must_== 0.62567138671875
      greenMatrixColumnTag(0).valueZ must_== 0.0608673095703125
    }
    "blueMatrixColumnTag" >> {
      val blueMatrixColumnTag = readTag("blueMatrixColumnTag", TypeXYZ)
      blueMatrixColumnTag.elements.list.length must_== 1
      blueMatrixColumnTag(0).valueX must_== 0.1491851806640625
      blueMatrixColumnTag(0).valueY must_== 0.0632171630859375
      blueMatrixColumnTag(0).valueZ must_== 0.74456787109375
    }
  }
}
