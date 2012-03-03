package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._

object TypeMultiLocalizedUnicode extends ElementBuilder[TypeMultiLocalizedUnicode] {
  val typeSignature = "mluc"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    val numNames = NumberU32(ins)
    val fields = EachField.times(ins, numNames.value.toInt)
    // それぞれの文字列を読み込む
    def readString(strings: List[MutiLocalizedString], sorted: List[EachField]): ExpressionList[MutiLocalizedString] = sorted match {
      case Nil => new ExpressionList(strings)
      case field :: fs => {
        val pos = (commons :: numNames :: fields :: strings)./:(0){ _ + _.length.toInt }
        val pad = ReadBytes(ins, field.offset - pos)
        val stringData = ReadBytes(ins, field.len)
        val mString = new MutiLocalizedString(field.language.value, field.country.value, pad, stringData)
        readString(mString :: strings, fs)
      }
    }
    val strings = readString( Nil, fields.list sort { _.offset < _.offset } )
    new TypeMultiLocalizedUnicode(commons, numNames, fields, strings)
  }
  object EachField extends Builder[EachField] {
    def apply(ins: InputStream) = {
      new EachField(
        NumberU16(ins),
        NumberU16(ins),
        NumberU32(ins),
        NumberU32(ins)
      )
    }
  }
  class EachField(val language: NumberU16, val country: NumberU16, val lengthOfString: NumberU32, val offsetToString: NumberU32) extends Expression {
    val bytes = consists(language, country, lengthOfString, offsetToString)
    val offset = offsetToString.value.toInt
    val len = lengthOfString.value.toInt
  }
  class MutiLocalizedString(val languageCode: Int, val countryCode: Int,
                            val pad: Bytes, val stringData: Bytes) extends Expression {
    val bytes = consists(pad, stringData)
    val string = languageCode match {
      // TODO 言語に合わせた文字列生成
      case v => new String(stringData.bytes.toArray)
    }
  }
}
class TypeMultiLocalizedUnicode(commons: TagElement.CommonHeads,
                                numNames: NumberU32,
                                fields: ExpressionList[TypeMultiLocalizedUnicode.EachField],
                                val strings: ExpressionList[TypeMultiLocalizedUnicode.MutiLocalizedString]) extends TagElement(commons, numNames, fields, strings) {
  def findString(language: Int, country: Int) = {
    strings.list find { x => x.languageCode == language & x.countryCode == country }
  }
}
