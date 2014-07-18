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

  def fill(x0:Int, y0:Int, r:Int, f:Cell=>Double): Seq[(Cell, Double)] = {
    val result = collection.mutable.Map[Cell, Double]()
    for(x <- -r to r;
        y <- -r to r) {
      if (x*x + y*y <= r*r + r*0.8) {
        val pt = (x0 + x, y0 + y)
        val d = f(pt)
        result += ((pt, d))
        ()
      }
    }
    result.toVector

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
    def inRange(t:Double, min:Double, max:Double) = {
      t >= min && t <= max
    }

    def angleToEdge(theta:Double):(Int,Int) = {
      val (roundX, roundY) = if (inRange(theta, 0, Math.PI/2)) {
        (math.floor _, math.ceil _)
      } else if (inRange(theta, Math.PI/2, Math.PI)) {
        (math.ceil _, math.ceil _)
      } else if (inRange(theta, Math.PI, 3*Math.PI/2)) {
        (math.ceil _, math.floor _)
      } else {
        (math.floor _, math.floor _)
      }
      val x = roundX(center.x + math.cos(theta) * radius).toInt
      val y = roundY(center.y + math.sin(theta) * radius).toInt
      (x, y)
    }
  }
