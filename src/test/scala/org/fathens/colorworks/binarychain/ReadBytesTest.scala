package org.fathens.colorworks.binarychain

import java.io._
import _root_.org.specs._

object ByteArray {
  implicit def range(seq: RandomAccessSeq.Projection[Int]): List[Byte] = range(seq:_*)
  def range(seq: Int*) = ( seq map { _.toByte } ).toList
  def newInputStream(array: List[Byte]) = new ByteArrayInputStream(array.toArray)
  def newInputStream(string: String): InputStream = newInputStream(string.getBytes.toList)
}
class ReadBytesTest extends SpecificationWithJUnit {
  import ByteArray._
  "デフォルトの読み込みサイズは1" in {
    val bytes = ReadBytes(newInputStream("ABC"))
    bytes.length must_== 1
  }
  "任意のサイズを指定できる" in {
    val bytes = ReadBytes(newInputStream("ABC"), 2)
    new String(bytes.bytes.toArray) must_== "AB"
  }
  "途中からの読み込み" in {
    val ins = newInputStream("ABCDEF")
    ins.read()
    val bytes = ReadBytes(ins, 3)
    new String(bytes.bytes.toArray) must_== "BCD"
  }
}
class BuilderTest extends SpecificationWithJUnit {
  import ByteArray._
  "読み込み過ぎてもエラーにならない" in {
    object Reader extends Builder[Bytes] {
      def apply(ins: InputStream) = {
        new Bytes(read(ins, 8)) // 8バイト読み込んでみる
      }
    }
    val ins = newInputStream("1234")
    val bytes = Reader(ins)
    bytes.length must_== 4
    new String(bytes.bytes.toArray) must_== "1234"
  }
  "最後まで読んだ後なら0バイトになる" in {
    object Reader extends Builder[Bytes] {
      def apply(ins: InputStream) = {
        new Bytes(read(ins, 8)) // 8バイト読み込んでみる
      }
    }
    val ins = newInputStream("123")
    ins.read; ins.read; ins.read
    ins.read must_== -1
    val bytes = Reader(ins)
    bytes.length must_== 0
  }
}
class ExpressionTest extends SpecificationWithJUnit {
  import ByteArray._
  class ThreePart(a: Bytes, b: Bytes, c: Bytes) extends Expression {
    val bytes = consists(a, b, c)
  }
  "複数から成り立つなら合計の大きさになる" in {
    val ins = newInputStream("ABDCDEFGHIJKLMN")
    val three = new ThreePart(ReadBytes(ins, 2), ReadBytes(ins, 3), ReadBytes(ins, 4))
    three.length must_== 9
  }
  "複数から成り立つ時はconsistsへの引数の順番になる" in {
    val ins = newInputStream("ABDCDEFGHIJKLMN")
    val three = new ThreePart(ReadBytes(ins, 3), ReadBytes(ins, 2), ReadBytes(ins, 5))
    new String(three.bytes.toArray) must_== "ABDCDEFGHI"
  }
  "入力ストリームは頭から" in {
    val ins = newInputStream("12345678")
    val three = new ThreePart(ReadBytes(ins, 2), ReadBytes(ins, 1), ReadBytes(ins, 3))
    val ni = three.newInputStream
    val buf = new Array[Byte](ni.available)
    ni.read(buf)
    new String(buf) must_== "123456"
  }
}
class ExpressionListTest extends SpecificationWithJUnit {
  import ByteArray._
  "リストは順番に連結される" in {
    val ins = newInputStream("123456789")
    val list = List(ReadBytes(ins, 3), ReadBytes(ins, 2), ReadBytes(ins, 4))
    val el = new ExpressionList(list)
    new String(el.bytes.toArray) must_== "123456789"
  }
  "リストへのアクセスはリストと同じ" in {
    val ins = newInputStream(0 until 10)
    val list = List(ReadBytes(ins, 3), ReadBytes(ins, 2), ReadBytes(ins, 4))
    val el = new ExpressionList(list)
    el(0).bytes must_== range(0, 1, 2)
    el(1).bytes must_== range(3, 4)
    el(2).bytes must_== range(5 until 9)
  }
}
