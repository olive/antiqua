package in.dogue.antiqua.data

import com.deweyvm.gleany.graphics.Color
import java.awt.image.BufferedImage
import in.dogue.antiqua.Antiqua
import Antiqua._
import java.io.File
import javax.imageio.ImageIO

class AugArray2d[T](arr:Array2d[T]) {
  def render(path:String)(f:T => Color) {
    def cl(c:Color) = com.badlogic.gdx.graphics.Color.rgb888(c.toLibgdxColor)
    val img = new BufferedImage(arr.cols, arr.rows, BufferedImage.TYPE_INT_RGB)
    arr.foreach { case (p, t) =>
      val c = f(t)
      img.setRGB(p.x, p.y, cl(c))
    }

    val out = new File(path)
    if (!ImageIO.write(img, "png", out)) {
      throw new RuntimeException()
    }
  }
}
