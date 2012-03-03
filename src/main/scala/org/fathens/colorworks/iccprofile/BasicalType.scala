package org.fathens.colorworks.iccprofile

import java.io._
import _root_.org.fathens.colorworks.binarychain._

// dateTimeNumber
object DateTimeNumber extends BuilderFixed[DateTimeNumber] {
  val length = NumberU16.length * 6
  def apply(ins: InputStream) = {
    new DateTimeNumber(
      NumberU16(ins),
      NumberU16(ins),
      NumberU16(ins),
      NumberU16(ins),
      NumberU16(ins),
      NumberU16(ins)
    )
  }
}
class DateTimeNumber(val year: NumberU16, val month: NumberU16, val day: NumberU16, val hours: NumberU16, val minutes: NumberU16, val seconds: NumberU16) extends Expression {
  val bytes = consists(year, month, day, hours, minutes, seconds)
  
  import java.util._
  def date = {
    val c = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
    c.set(year.value, month.value - 1, day.value, hours.value, minutes.value, seconds.value)
    c.getTime
  }
}

//response16Number
object Response16Number extends BuilderFixed[Response16Number] {
  val length = NumberU16.length + 2 + NumberS15Fixed16.length
  def apply(ins: InputStream) = {
    new Response16Number(
      NumberU16(ins),
      ReadBytes(ins, 2),
      NumberS15Fixed16(ins)
    )
  }
}
class Response16Number(val deviceIndex: NumberU16, reserved: Bytes, val measurementValue: NumberS15Fixed16) extends Expression {
  val bytes = consists(deviceIndex, reserved, measurementValue)

  val index = deviceIndex.value
  val value = measurementValue.value
}

// XYZNumber
object XYZNumber extends BuilderFixed[XYZNumber] {
  val length = NumberS15Fixed16.length * 3
  def apply(ins: InputStream) = {
    new XYZNumber(
      NumberS15Fixed16(ins),
      NumberS15Fixed16(ins),
      NumberS15Fixed16(ins)
    )
  }
}
class XYZNumber(val x: NumberS15Fixed16, val y: NumberS15Fixed16, val z: NumberS15Fixed16) extends Expression {
  val bytes = consists(x, y, z)
  
  val valueX = x.value
  val valueY = y.value
  val valueZ = z.value
}

// 7 bit string (ASCII)
object String7bit extends Builder[String7bit] {
  // Read until 00h
  def apply(ins: InputStream) = {
    def until0(l: List[Byte]): List[Byte] = ins.read match {
      case 0 => 0.toByte :: l
      case -1 => l
      case i => until0(i.toByte :: l)
    }
    new String7bit(until0(Nil).reverse)
  }
  def apply(ins: InputStream, length: Int) = {
    new String7bit(read(ins, length))
  }
}
class String7bit(val bytes: List[Byte]) extends Expression {
  val lengthOfString = bytes.zipWithIndex.find { _._1 == 0 } match {
    case None => length.toInt
    case Some(v) => v._2
  }
  val string = new String(bytes.take(lengthOfString).toArray)
}
