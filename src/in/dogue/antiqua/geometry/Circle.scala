package in.dogue.antiqua.geometry

import scala.collection.mutable.ArrayBuffer

import in.dogue.antiqua.Antiqua
import Antiqua._


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

  case class Circle(center:(Int,Int), radius:Int) {
    @inline def x = center.x
    @inline def y = center.y
    @inline def r = radius
    def contains(ij:(Int,Int)) = {
      val x = ij.x - center.x
      val y = ij.y - center.y
      x*x + y*y <= radius*radius

    }

    def angleToEdge(theta:Double):(Int,Int) = {
      val x = math.round(center.x + math.cos(theta) * radius).toInt
      val y = math.round(center.y + math.sin(theta) * radius).toInt
      (x, y)
    }
  }
