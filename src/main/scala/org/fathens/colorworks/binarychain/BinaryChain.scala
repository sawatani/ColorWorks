package org.fathens.colorworks.binarychain

import java.io._

trait Builder[T <: Expression] {
  def apply(ins: InputStream): T
  def read(ins: InputStream, length: Int): List[Byte] = {
    val bs = new Array[Byte](length)
    val len = ins.read(bs)
    bs.take(len).toList
  }
  def times(ins: InputStream, n: Int) = {
    val xs = (0 until n) map { i => apply(ins) }
    new ExpressionList(xs.toList)
  }
}
trait BuilderFixed[T <: Expression] extends Builder[T] {
  val length: Long
}
trait Expression {
  val bytes: List[Byte]
  lazy val length: Long = bytes.length.toLong
  def consists(elements: Expression*) = {
    (elements.toList :\ List[Byte]()) { _.bytes ::: _ }
  }
  def newInputStream = new ByteArrayInputStream(bytes.toArray)
}

trait WithSuccessor[S <: Expression] extends Expression {
  def next: Option[S]
}

object ReadBytes extends Builder[Bytes] {
  def apply(ins: InputStream) = apply(ins, 1)
  def apply(ins: InputStream, length: Int) = new Bytes(read(ins, length))
}
class Bytes(val bytes: List[Byte]) extends Expression

class ExpressionList[T <: Expression](val list: List[T]) extends Expression {
  val bytes = consists(list:_*)
  def apply(index: Int) = list(index)
}
