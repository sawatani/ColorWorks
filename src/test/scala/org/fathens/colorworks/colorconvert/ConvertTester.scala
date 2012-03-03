package org.fathens.colorworks.colorconvert

object ConvertTester {
  def main(args: Array[String]) {
    val srcCS = ColorSpace(args(0))
    val imageFile = new java.io.File(args(1))
    val csvFile = new java.io.File(args(2))
    val numX = args(3).toInt
    val numY = args(4).toInt
    // 画像の読み込み
    val image = {
      import java.awt.image._
      val p = javax.media.jai.JAI.create("fileload", imageFile.getAbsolutePath)
      val cm = new ComponentColorModel(srcCS.CS, false, false, 1, DataBuffer.TYPE_BYTE)
      new BufferedImage(cm, p.copyData, false, null)
    }
    val cellWidth = image.getWidth / numX
    val cellHeight = image.getHeight / numY
    val numComponents = image.getColorModel.getNumColorComponents
    require(numComponents == srcCS.numComponents)
    // 各セルの色を取り出しLabに変換
    val listColors = for {
      posX <- (0 until numX).map(_ * cellWidth + cellWidth / 2)
      posY <- (0 until numY).map(_ * cellHeight + cellHeight / 2)
      pixel = image.getData.getPixel(posX, posY, new Array[Double](numComponents))
      color = {
        val v = pixel.map(_ / 0xff).toList
        println("Pixel(%d, %d) = %s(%s)".format(posX, posY, pixel.mkString(","), v.map(_ * 100).map(_.round).mkString(",")))
        srcCS.color(v)
      }
    } yield XYZ.getInstance(color).toLab
    // CSVに出力
    val writer = new java.io.PrintWriter(csvFile)
    try {
      writer.println("index,L,a,b")
      listColors.zipWithIndex.foreach {
        case (lab, index) =>
          val line = "%d,%02.3f,%02.3f,%02.3f".format(index, lab.value.l, lab.value.a, lab.value.b)
          writer.println(line)
      }
    } finally {
      writer.close()
    }
  }
}