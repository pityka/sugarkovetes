package pityu.sugar

import java.awt._
import java.awt.image._
import javax.swing._

object convertRGBToInt {
  def apply(r: Int, g: Int, b: Int, a: Int): Int = ((a & 0xFF) << 24) | //alpha
    ((r & 0xFF) << 16) | //red
    ((g & 0xFF) << 8) | //green
    ((b & 0xFF) << 0); //blue
}

object arrayToImage {
  def apply(mat: Array[Array[Int]]) = {
    val image = new BufferedImage(mat.size, mat.head.size,
      BufferedImage.TYPE_INT_ARGB);
    for (i <- 0 until mat.length; j <- 0 until mat.length) {
      image.setRGB(i, j, mat(i)(j))
    }
    image
  }
}

class Window(width: Int, h: Int) {

  val frame = new javax.swing.JFrame();

  frame.setSize(1000, 1000);
  frame.setVisible(true);

  val image = new BufferedImage(width, h,
    BufferedImage.TYPE_INT_ARGB);

  val picLabel = new JLabel(new ImageIcon(image))
  frame.add(picLabel)

  def setImage(mat: Array[Array[Int]]) = {
    for (i <- 0 until mat.length; j <- 0 until mat.length) {
      image.setRGB(i, j, mat(i)(j))
    }
    picLabel.setIcon(new ImageIcon(image))
  }

  def setImage(bi: BufferedImage) = {
    picLabel.setIcon(new ImageIcon(bi))
  }

  def close {
    frame.setVisible(false)
    picLabel.setVisible(false)
    frame.dispose
  }
}

