package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.specs2.mutable._

class TagElementTest extends Specification {
  implicit def charToInt(c: Char) = c.toInt
  implicit def intsToBytes(ints: Seq[Int]) = new {
    def toBytes = ints map { _.toByte }
  }
  def input(ints: Int*) = new ByteArrayInputStream(ints.toBytes.toArray)
  "TagTypeのSignatureの長さは4バイト" >> {
    TagTypeSignature.length must_== 4
  }
  "commonsはシグネーチャと予約との8バイト" >> {
    val ins = input('a', 'b', 'c', 'd', 1, 2, 3, 4, 5, 6, 7, 8)
    val e = TagElement(ins, 0, 12)
    e.length must_== 12
    e.commons.length must_== 8
    e.commons.pad.length must_== 0
    e.commons.typeSignature.name must_== "abcd"
    e.commons.reserved.bytes must_== List(1, 2, 3, 4).toBytes
    e.parts.length must_== 1
    e.parts(0).bytes must_== List(5, 6, 7, 8).toBytes
  }
  "padを指定すればcommonsに含まれる。しかし、elementの長さには含まれない。" >> {
    val ins = input(10, 11, 'a', 'b', 'c', 'd', 1, 2, 3, 4, 5, 6, 7, 8)
    val e = TagElement(ins, 2, 12)
    e.length must_== 12
    e.commons.length must_== 8
    e.commons.pad.bytes must_== List(10, 11).toBytes
    e.commons.typeSignature.name must_== "abcd"
    e.commons.reserved.bytes must_== List(1, 2, 3, 4).toBytes
    e.parts.length must_== 1
    e.parts(0).bytes must_== List(5, 6, 7, 8).toBytes
  }
}
