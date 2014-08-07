package in.dogue.antiqua.geometry

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Graph
import scala.util.Random
import com.deweyvm.gleany.data.Point2d

object StarPolygon {

  def create(center:Cell, numPoints:Int, rad:Int, r:Random) = {
    val pts = (0 until numPoints).map { p =>
      val angle = (p/numPoints.toDouble)*2*math.Pi
      val rd = rad/2 + r.nextInt(rad/2)
      val x = rd*math.cos(angle)
      val y = rd*math.sin(angle)
      (x, y).map{_.toInt} |+| center
    }
    pts match {
      case x +: y +: z +: xs => fromPoints(center, x, y, z, xs)
      case _ => throw new RuntimeException() //fixme
    }
  }

  private def fromPoints(center:Cell, p1:Cell, p2:Cell, p3:Cell, rest:Seq[Cell]) = {
    val all = p1 +: p2 +: p3 +: rest
    val lines = for ((p, q) <- all.pairwise) yield {
      Line.bresenhamTup(p, q)
    }
    StarPolygon(center, all, lines)
  }
}

case class StarPolygon private (center:Cell, points:Seq[Cell], lines:Seq[Seq[Cell]]) {
  def toPolygon:Polygon = Polygon.fromPoints(points.map{case (i, j) => Point2d(i, j)}.toVector).get
  def getCells:Set[Cell] = {
    val cells = lines.flatten.toSet
    val g = new Graph[Cell, Cell] {
      def getNeighbors(c:Cell):Seq[Cell] = {
        def getN(t:Cell) = t.onlyIfs(!cells.contains(t))
        val cs = for (cell <- List((-1, 0), (1, 0), (0, 1), (0, -1))) yield {
          getN(cell |+| c)
        }
        cs.flatten
      }
      def get(c:Cell)  = c
    }
    Graph.flood(g, center)
  }
}
