package in.dogue.antiqua.geometry

import in.dogue.antiqua.Antiqua
import Antiqua._

case class Ellipse(rx:Double, ry:Double, cx:Double, cy:Double) {
  private def sq(x:Double) = x*x
  def contains(ij:(Int,Int)) = {
    val x = ij.x
    val y = ij.y
    sq(x - cx)/sq(rx) + sq(y - cy)/sq(ry) <= 1
  }

  def getY(x:Double):Seq[Double] = {
    val rx2 = sq(rx)
    val ry2 = sq(ry)
    val t = (rx2*ry2 - ry2*sq(x - cx))/rx2
    val st = t.sqrt
    Seq(st, -st)
  }

  def center:(Double,Double) = (cx, cy)
}
