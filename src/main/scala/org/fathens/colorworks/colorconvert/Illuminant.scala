package org.fathens.colorworks.colorconvert

object Illuminant {
  implicit def toupleToWhitepoint(t: (Double, Double, Double)): Illuminant = {
    new Illuminant(CIEXYZ(t._1, t._2, t._3))
  }
  val D50: Illuminant = (96.4212, 100.0, 82.5188)
  val D55: Illuminant = (95.6797, 100.0, 92.1481)
  val D65: Illuminant = (95.0429, 100.0, 108.8900)
  val D70: Illuminant = (94.9722, 100.0, 122.6394)
}
class Illuminant(values: CIEXYZ) extends CIEXYZ(values.x, values.y, values.z)
