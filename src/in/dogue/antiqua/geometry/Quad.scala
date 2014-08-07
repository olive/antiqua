package in.dogue.antiqua.geometry

import in.dogue.antiqua.Antiqua
import Antiqua._

class Quad(p0:Cell, p1:Cell, p2:Cell, p3:Cell) {
  def fill:Seq[Cell] = {
    val ls = (Line.bresenhamTup(p0, p1)
                 ++ Line.bresenhamTup(p1, p2)
                 ++ Line.bresenhamTup(p2, p3)
                 ++ Line.bresenhamTup(p3, p0))
    val max = ls.maxBy(_._2)
    val min = ls.minBy(_._2)
    val g = ls.groupBy(_._2)
    val minMap = g.map {case (y, cells) => y -> cells.minBy(_._1)}
    val maxMap = g.map {case (y, cells) => y -> cells.maxBy(_._1)}
    (for (j <- min.y to max.y) yield {
      (minMap(j).x to maxMap(j).x).map { i => (i, j)}
    }).flatten
  }
}
