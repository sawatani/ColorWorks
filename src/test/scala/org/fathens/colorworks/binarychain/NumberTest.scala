package org.fathens.colorworks.binarychain

import java.io._
import _root_.org.specs2._

class NumberTest extends SpecificationWithJUnit("BasicalNumberの読み込み") {
  implicit def range(seq: RandomAccessSeq.Projection[Int]): List[Byte] = range(seq:_*)
  implicit def toByte(i: Int) = (i.toByte) & 0xff
  def range(seq: Int*) = ( seq map { _.toByte } ).toList
  def newInputStream(seq: Int*): InputStream = newInputStream(range(seq:_*))
  def newInputStream(array: List[Byte]) = new ByteArrayInputStream(array.toArray)
  
  def dump(b: Expression) = println(b.bytes map { _ & 0xff })
  
  "Unsigned int 8-bit" should {
    "長さは1バイト" >> {
      "参照数" >> { NumberU8.length must_== 1 }
      "実際の読み込み" >> {
        val ins = newInputStream(0, 1)
        NumberU8(ins).length must_== 1
      }
    }
    "8bitの符号なし" >> {
      val ins = newInputStream(210, 2, 5)
      NumberU8(ins).value must_== 210.toByte
    }
  }
  "Unsigned int 16-bit" should {
    "長さは2バイト" >> {
      "参照数" >> { NumberU16.length must_== 2 }
      "実際の読み込み" >> {
        val ins = newInputStream(0, 1)
        NumberU16(ins).length must_== 2
      }
    }
    "16bitのBigEndianで符号なし" >> {
      val ins = newInputStream(210, 21, 111, 222)
      NumberU16(ins).value must_== (210 * 256 + 21)
      NumberU16(ins).value must_== (111 * 256 + 222)
    }
  }
  "Unsigned int 32-bit" should {
    "長さは4バイト" >> {
      "参照" >> { NumberU32.length must_== 4 }
      "実際の読み込み" >> {
        val ins = newInputStream(0, 1, 8, 15, 88)
        NumberU32(ins).length must_== 4
      }
    }
    """32bitのBigEndianで符号なし""" >> {
      val ins = newInputStream(210, 21, 111, 222)
      NumberU32(ins).value must_== (BigInt(210) * 256 * 256 * 256
                                 + (BigInt( 21) * 256 * 256)
                                 + (BigInt(111) * 256)
                                 +  BigInt(222))
    }
  }
  "Unsigned int 64-bit" should {
    "長さは8バイト" >> {
      "参照" >> { NumberU64.length must_== 8 }
      "実際の読み込み" >> {
        val ins = newInputStream(240, 112, 8, 15, 88, 22, 220, 2, 9, 23)
        NumberU64(ins).length must_== 8
      }
    }
    "64bitのBigEndianで符号なし" >> {
      val ins = newInputStream(200, 71, 50, 100, 210, 21, 111, 222)
      NumberU64(ins).value must_== (BigInt(200) * 256 * 256 * 256 * 256 * 256 * 256 * 256
                                 + (BigInt( 71) * 256 * 256 * 256 * 256 * 256 * 256)
                                 + (BigInt( 50) * 256 * 256 * 256 * 256 * 256)
                                 + (BigInt(100) * 256 * 256 * 256 * 256)
                                 + (BigInt(210) * 256 * 256 * 256)
                                 + (BigInt( 21) * 256 * 256)
                                 + (BigInt(111) * 256)
                                 +  BigInt(222))
    }
  }
  // 固定少数点
  "符号付き固定小数点 整数部 15-bit, 小数部 16-bit" should {
    "長さは4バイト" >> {
      "参照" >> { NumberS15Fixed16.length must_== 4 }
      "実際の読み込み" >> {
        val ins = newInputStream(0 until 10)
        NumberS15Fixed16(ins).length must_== 4
      }
    }
    "小数部の分母は16bit" >> {
      NumberS15Fixed16.decimal_denominator must_== 65536
    }
    "符号有りで15bit+16bit" >> {
      "最高bitが符号" >> {
        val ins = newInputStream(0x80, 0x00, 0x00, 0x00)
        val d = NumberS15Fixed16(ins)
        d.integral must_== -32768
        d.decimal_numerator must_== 0
        d.decimal_denominator must_== 65536
        d.value must_== -32768.0
      }
      "0が続けばゼロ" >> {
        val ins = newInputStream(0x00, 0x00, 0x00, 0x00)
        val d = NumberS15Fixed16(ins)
        d.integral must_== 0
        d.decimal_numerator must_== 0
        d.decimal_denominator must_== 65536
        d.value must_== 0
      }
      "2バイト目までが整数" >> {
        val ins = newInputStream(0x00, 0x01, 0x00, 0x00)
        val d = NumberS15Fixed16(ins)
        d.integral must_== 1
        d.decimal_numerator must_== 0
        d.decimal_denominator must_== 65536
        d.value must_== 1.0
      }
      "小数部は16bitのMAX値で割る" >> {
        val ins = newInputStream(0x7F, 0xFF, 0xFF, 0xFF)
        val d = NumberS15Fixed16(ins)
        d.integral must_== 32767
        d.decimal_numerator must_== 65535
        d.decimal_denominator must_== 65536
        d.value must_== 32767 + (65535.toDouble/65536)
      }
    }
  }
  "符号なし固定小数点 整数部 16-bit, 小数部 16-bit" should {
    "長さは4バイト" >> {
      "参照" >> { NumberU16Fixed16.length must_== 4 }
      "実際の読み込み" >> {
        val ins = newInputStream(0 until 10)
        NumberU16Fixed16(ins).length must_== 4
      }
    }
    "小数部の分母は16bit" >> {
      NumberU16Fixed16.decimal_denominator must_== 65536
    }
    "符号無しで16bit+16bit" >> {
      "符号はなし" >> {
        val ins = newInputStream(0x80, 0x00, 0x00, 0x00)
        val d = NumberU16Fixed16(ins)
        d.integral must_== 32768
        d.decimal_numerator must_== 0
        d.decimal_denominator must_== 65536
        d.value must_== 32768.0
      }
      "0が続けばゼロ" >> {
        val ins = newInputStream(0x00, 0x00, 0x00, 0x00)
        val d = NumberU16Fixed16(ins)
        d.integral must_== 0
        d.decimal_numerator must_== 0
        d.decimal_denominator must_== 65536
        d.value must_== 0
      }
      "2バイト目までが整数" >> {
        val ins = newInputStream(0x00, 0x01, 0x00, 0x00)
        val d = NumberU16Fixed16(ins)
        d.integral must_== 1
        d.decimal_numerator must_== 0
        d.decimal_denominator must_== 65536
        d.value must_== 1.0
      }
      "小数部は16bitのMAX値で割る" >> {
        val ins = newInputStream(0xFF, 0xFF, 0xFF, 0xFF)
        val d = NumberU16Fixed16(ins)
        d.integral must_== 65535
        d.decimal_numerator must_== 65535
        d.decimal_denominator must_== 65536
        d.value must_== 65535 + (65535.toDouble/65536)
      }
    }
  }
  "符号なし固定小数点 整数部 1-bit, 小数部 15-bit" should {
    "長さは4バイト" >> {
      "参照" >> { NumberU1Fixed15.length must_== 2 }
      "実際の読み込み" >> {
        val ins = newInputStream(0 until 10)
        NumberU1Fixed15(ins).length must_== 2
      }
    }
    "小数部の分母は15bit" >> {
      NumberU1Fixed15.decimal_denominator must_== 32768
    }
    "符号無しで1bit+15bit" >> {
      "0が続けばゼロ" >> {
        val ins = newInputStream(0x00, 0x00)
        val d = NumberU1Fixed15(ins)
        d.integral must_== 0
        d.decimal_numerator must_== 0
        d.decimal_denominator must_== 32768
        d.value must_== 0
      }
      "1ビット目だけが整数" >> {
        val ins = newInputStream(0x80, 0x00)
        val d = NumberU1Fixed15(ins)
        d.integral must_== 1
        d.decimal_numerator must_== 0
        d.decimal_denominator must_== 32768
        d.value must_== 1.0
      }
      "小数部は15bitのMAX値で割る" >> {
        val ins = newInputStream(0xFF, 0xFF)
        val d = NumberU1Fixed15(ins)
        d.integral must_== 1
        d.decimal_numerator must_== 32767
        d.decimal_denominator must_== 32768
        d.value must_== 1 + (32767.toDouble/32768)
      }
    }
  }
  "符号なし固定小数点 整数部 8-bit, 小数部 8-bit" should {
    "長さは2バイト" >> {
      "参照" >> { NumberU8Fixed8.length must_== 2 }
      "実際の読み込み" >> {
        val ins = newInputStream(0 until 10)
        NumberU1Fixed15(ins).length must_== 2
      }
    }
    "小数部の分母は8bit" >> {
      NumberU8Fixed8.decimal_denominator must_== 256
    }
    "符号無しで8bit+8bit" >> {
      "0が続けばゼロ" >> {
        val ins = newInputStream(0x00, 0x00)
        val d = NumberU8Fixed8(ins)
        d.integral must_== 0
        d.decimal_numerator must_== 0
        d.decimal_denominator must_== 256
        d.value must_== 0
      }
      "1バイト目が整数" >> {
        val ins = newInputStream(0x01, 0x00)
        val d = NumberU8Fixed8(ins)
        d.integral must_== 1
        d.decimal_numerator must_== 0
        d.decimal_denominator must_== 256
        d.value must_== 1.0
      }
      "小数部は8bitのMAX値で割る" >> {
        val ins = newInputStream(0xFF, 0xFF)
        val d = NumberU8Fixed8(ins)
        d.integral must_== 255
        d.decimal_numerator must_== 255
        d.decimal_denominator must_== 256
        d.value must_== 255 + (255.toDouble/256)
      }
    }
  }
}
