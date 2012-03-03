package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.specs2.mutable._

class TagTableTest extends Specification {
  def newInputStream(is: Int*) = new ByteArrayInputStream(is.map{_.toByte}.toArray)
  "レコード数が0も許す" >> {
    val ins = newInputStream(0, 0, 0, 0)
    val t = TagTable(ins)
    t.count must_== 0
  }
  "レコード数に応じて長さが変わる(1の場合)" >> {
    val ins = newInputStream(0, 0, 0, 1, 49, 50, 51, 52, 0, 0, 0, 1, 0, 0, 0, 2)
    val t = TagTable(ins)
    t.count must_== 1
    t.length must_== 4 + 12
    t.tags.list.length must_== t.count
    t.sortedTags.length must_== t.count
  }
  "レコード数に応じて長さが変わる(2の場合)" >> {
    val ins = newInputStream(0, 0, 0, 2, 49, 50, 51, 52, 0, 0, 0, 1, 0, 0, 0, 8, 53, 54, 55, 56, 0, 0, 0, 10, 0, 0, 0, 9)
    val t = TagTable(ins)
    t.count must_== 2
    t.length must_== 4 + 12 * 2
    t.tags.list.length must_== t.count
    t.sortedTags.length must_== t.count
  }
  "レコードはオフセット順にソートされる" >> {
    val ins = newInputStream(0, 0, 0, 3, 49, 50, 51, 52, 0, 0, 0, 1, 0, 0, 0, 8, 53, 54, 55, 56, 0, 0, 0, 123, 0, 0, 0, 9, 65, 50, 66, 49, 0, 0, 0, 100, 0, 0, 0, 90)
    val t = TagTable(ins)
    t.count must_== 3
    t.length must_== 4 + 12 * 3
    t.tags.list.length must_== t.count
    t.sortedTags.length must_== t.count
    t.sortedTags(0).tagName must_== "1234"
    t.sortedTags(0).offset must_== 1
    t.sortedTags(0).size must_== 8
    t.sortedTags(1).tagName must_== "AToB1Tag"
    t.sortedTags(1).offset must_== 100
    t.sortedTags(1).size must_== 90
    t.sortedTags(2).tagName must_== "5678"
    t.sortedTags(2).offset must_== 123
    t.sortedTags(2).size must_== 9
  }
}

class TagRecordTest extends Specification {
  def newInputStream(is: Int*) = new ByteArrayInputStream(is.map{_.toByte}.toArray)
  "固定長" >> {
    TagRecord.length must_== 12
    val ins = newInputStream(49, 50, 51, 52, 0, 0, 0, 1, 0, 0, 0, 2)
    val r = TagRecord(ins)
    r.length must_== 12
  }
  "長さが満たなければエラーになる" >> {
    val ins = newInputStream(49, 50, 51, 52, 0, 1, 0, 2)
    TagRecord(ins) must throwA[IllegalArgumentException]
  }
  "シグネーチャと数値が2つの組合せ" >> {
    val ins = newInputStream(65, 50, 66, 49, 0, 0, 0, 1, 0, 0, 0, 2)
    val r = TagRecord(ins)
    r.tagName must_== "AToB1Tag"
    r.offset must_== 1
    r.size must_== 2
  }
}

class TagSignatureTest extends Specification {
  def newInputStream(is: Int*) = new ByteArrayInputStream(is.map{_.toByte}.toArray)
  "必ず4文字の文字列" in {
    TagSignature.length must_== 4
    val ins = newInputStream(49, 50, 51, 52, 53, 54)
    val s = TagSignature(ins)
    s.length must_== 4
    s.lengthOfString must_== 4
    s.string must_== "1234"
  }
  "長さが満たなければエラーになる" in {
    TagSignature.length must_== 4
    val ins = newInputStream(49, 50, 51)
    TagSignature(ins) must throwA[IllegalArgumentException]
  }
  "必ず4文字の文字列だが、途中でNULL(0)があった場合はそこまでとなる" in {
    val ins = newInputStream(49, 50, 0, 51, 52, 53, 54)
    val s = TagSignature(ins)
    s.length must_== 4
    s.lengthOfString must_== 2
    s.string must_== "12"
  }
  "定義されているシグネーチャなら正式名称が取り出せる" in {
    val ins = newInputStream(65, 50, 66, 48)
    val s = TagSignature(ins)
    s.length must_== 4
    s.lengthOfString must_== 4
    s.string must_== "A2B0"
    s.name must_== "AToB0Tag"
  }
  "定義されていないシグネーチャならそのまま返る" in {
    val ins = newInputStream(65, 66, 67, 68)
    val s = TagSignature(ins)
    s.length must_== 4
    s.lengthOfString must_== 4
    s.string must_== "ABCD"
    s.name must_== "ABCD"
  }
}
