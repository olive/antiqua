package in.dogue.antiqua.data

import in.dogue.antiqua.Antiqua
import Antiqua._

class AugIntPair(p:(Int,Int)) {
  @inline def x = p._1
  @inline def y = p._2
  @inline def inRange(rect:(Int,Int,Int,Int)) = {
    x >= rect._1 && y >= rect._2 && x < rect._1 + rect._3 && y < rect._2 + rect._4
  }
  @inline def |+|(pr:(Int,Int)) = (p._1 + pr._1, p._2 + pr._2)
  @inline def |+(i:Int) = (p._1 + i, p._2)
  @inline def +|(j:Int) = (p._1, p._2 + j)

  @inline def |-|(pr:(Int,Int)) = (p._1 - pr._1, p._2 - pr._2)
  @inline def |-(i:Int) = (p._1 - i, p._2)
  @inline def -|(j:Int) = (p._1, p._2 - j)

  @inline def abs = (math.abs(p._1), math.abs(p._2))
  @inline def signum = (math.signum(p._1), math.signum(p._2))

  @inline def mag = (x*x + y*y).sqrt
  @inline def mag2 = x*x + y*y

  @inline def -->(d:Direction) = (p._1 + d.dx, p._2 + d.dy)
}
