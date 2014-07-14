package in.dogue.antiqua.geometry

import scala.collection.mutable.ArrayBuffer

import com.deweyvm.gleany.data.Point2d

object Circle {
  def bresenham(x0: Int, y0: Int, r: Int): Seq[(Int,Int)] = {
    var x = r
    var y = 0
    var rError = 1 - x

    val output = ArrayBuffer[(Int,Int)]()

    while(x >= y) {
      output += ((x + x0, y + y0))
      output += ((y + x0, x + y0))
      output += ((-x + x0, y + y0))
      output += ((-y + x0, x + y0))
      output += ((-x + x0, -y + y0))
      output += ((-y + x0, -x + y0))
      output += ((x + x0, -y + y0))
      output += ((y + x0, -x + y0))
      y += 1
      if (rError < 0) rError += 2*y + 1 else {
        x -= 1
        rError += 2*(y-x + 1)
      }
    }

    output
  }
}
