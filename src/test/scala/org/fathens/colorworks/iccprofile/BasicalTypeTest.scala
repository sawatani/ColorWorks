package org.fathens.colorworks.iccprofile

import java.io._
import _root_.org.specs2.mutable._

class DateTimeTest extends Specification {
  def withShort(is: Int*) = new ByteArrayInputStream(is.map{ i => List(i/256, i%256) }.:\(List[Int]()){_ ::: _}.map{_.toByte}.toArray)
  def newInputStream(is: Int*) = new ByteArrayInputStream(is.map{_.toByte}.toArray)
  "長さは固定" in {
    DateTimeNumber.length must_== 12
    val ins = newInputStream((0 until 100):_*)
    DateTimeNumber(ins).length must_== 12
  }
  "読み込んだ日付をGMTとして、ローカルタイム(SGT)に変換する" in {
    import java.util._
    val dt = TimeZone.getDefault
    TimeZone setDefault TimeZone.getTimeZone("Asia/Singapore")
    // シンガポール時刻でテストする
    val ins = withShort(2010, 3, 17, 13, 42, 18)
    ins.available must_== 12
    val d = DateTimeNumber(ins)
    val f = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    f.format(d.date) must_== "2010-03-17 21:42:18"
    // 元のタイムゾーンに戻す
    TimeZone.setDefault(dt)
  }
}

class Response16Test extends Specification {
  def newInputStream(is: Int*) = new ByteArrayInputStream(is.map{_.toByte}.toArray)
  "長さは固定" in {
    Response16Number.length must_== 8
    val ins = newInputStream((0 until 100):_*)
    Response16Number(ins).length must_== 8
  }
  "indexと値との対を格納する" in {
    val ins = newInputStream(0, 1, 0, 0, 0, 2, 0, 0)
    val r = Response16Number(ins)
    r.index must_== 1
    r.value must_== 2
  }
}

class XYZTest extends Specification {
  def newInputStream(is: Int*) = new ByteArrayInputStream(is.map{_.toByte}.toArray)
  "長さは固定" in {
    XYZNumber.length must_== 12
    val ins = newInputStream((0 until 100):_*)
    XYZNumber(ins).length must_== 12
  }
  "indexと値との対を格納する" in {
    val ins = newInputStream(0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0)
    val r = XYZNumber(ins)
    r.valueX must_== 1
    r.valueY must_== 2
    r.valueZ must_== 3
  }
}

class String7bitTest extends Specification {
  def newInputStream(is: Int*) = new ByteArrayInputStream(is.map{_.toByte}.toArray)
  "長さは固定ではなく、NULL(0)まで" in {
    val ins = newInputStream(32, 32, 32, 0, 32)
    String7bit(ins).length must_== 4
  }
  "NULL(0)までの、NULL(0)を含めない文字列になる" in {
    val ins = newInputStream(49, 50, 51, 52, 53, 0, 54, 55, 56, 57, 58)
    val r = String7bit(ins)
    r.length must_== 6
    r.lengthOfString must_== 5
    r.string must_== "12345"
  }
  "NULL(0)が無ければ最後までの文字列になる" in {
    val ins = newInputStream(49, 50, 51, 52, 53, 54, 55, 56, 57)
    val r = String7bit(ins)
    r.length must_== 9
    r.lengthOfString must_== 9
    r.string must_== "123456789"
  }
  "長さを指定して、NULL(0)が無ければその長さまでの文字列になる" in {
    val ins = newInputStream(49, 50, 51, 52, 53, 0, 54, 55, 56, 57, 58)
    val r = String7bit(ins, 3)
    r.length must_== 3
    r.lengthOfString must_== 3
    r.string must_== "123"
  }
  "長さを指定してすればバイト列はその長さになるが、NULL(0)が途中にあればそこまでの文字列になる" in {
    val ins = newInputStream(49, 50, 51, 52, 53, 0, 54, 55, 56, 57, 58)
    val r = String7bit(ins, 8)
    r.length must_== 8
    r.lengthOfString must_== 5
    r.string must_== "12345"
  }
  "指定した長さに満たなければそこまでの文字列となる" in {
    val ins = newInputStream(49, 50, 51, 52)
    val r = String7bit(ins, 7)
    r.length must_== 4
    r.lengthOfString must_== 4
    r.string must_== "1234"
  }
}
