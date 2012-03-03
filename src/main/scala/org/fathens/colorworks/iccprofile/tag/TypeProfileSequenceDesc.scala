package org.fathens.colorworks.iccprofile.tag

import java.io._
import _root_.org.fathens.colorworks.binarychain._
import _root_.org.fathens.colorworks.iccprofile._

object TypeProfileSequenceDesc extends ElementBuilder[TypeProfileSequenceDesc] {
  val typeSignature = "pseq"
  def build(commons: TagElement.CommonHeads, ins: InputStream, length: Long) = {
    val count = NumberU32(ins)
    new TypeProfileSequenceDesc(commons, count, ProfileDescription.times(ins, count.value.toInt))
  }
  object ProfileDescription extends Builder[ProfileDescription] {
    def apply(ins: InputStream) = {
      val sigMft = String7bit(ins, 4)
      val sigMdl = String7bit(ins, 4)
      val atts = DeviceAttributes(ins)
      val tech = String7bit(ins, 4)
      val mft = TypeMultiLocalizedUnicode(ins)
      val mdl = TypeMultiLocalizedUnicode(ins)
      new ProfileDescription(sigMft, sigMdl, atts, tech, mft, mdl)
    }
  }
  class ProfileDescription(val manufacturerSignature: String7bit,
                           val modelSignature: String7bit,
                           val attributes: DeviceAttributes,
                           val deviceTechnology: String7bit,
                           val manufacturer: TypeMultiLocalizedUnicode,
                           val model: TypeMultiLocalizedUnicode) extends Expression {
    val bytes = consists(manufacturerSignature, modelSignature, attributes, deviceTechnology, manufacturer, model)
  }
}
class TypeProfileSequenceDesc(commons: TagElement.CommonHeads,
                              count: NumberU32,
                              descs: ExpressionList[TypeProfileSequenceDesc.ProfileDescription]) extends TagElement(commons, count, descs) {

}
