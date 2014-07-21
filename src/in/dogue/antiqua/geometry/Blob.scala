package in.dogue.antiqua.geometry

import in.dogue.antiqua.data.Array2d
import com.deweyvm.gleany.data.{Recti, Point2d}
import scala.util.Random
import in.dogue.antiqua.Antiqua
import Antiqua._
/**
 * A blob is a connected (maybe convex) polygon on a 2d grid.
 */
object Blob {
  def create(cols:Int, rows:Int, numPoints:Int, threshold:Int, r:Random):Blob[Boolean] = {
    var found = false
    var count = 0
    var poly:Polygon = null
    var mask:Array2d[Boolean] = null
    while (!found) {
      val points = (0 until numPoints).map {
        (i:Int) => Point2d(r.nextInt(cols), r.nextInt(rows))
      }.sortBy{
        case pt => Math.atan2(pt.y, pt.x)
      }
      Polygon.fromPoints(points.toVector) match {
        case Some(p) =>
          poly = p
          found = true
        case None =>
          ()
      }

      mask = Array2d.tabulate(cols, rows) { case (i, j) =>
        poly.contains(Point2d(i, j))
      }

      if (mask.count{case (_, b) => b} < threshold) {
        found = false
      }
      count += 1
      if (count > 100) {
        println("Warning: degenerate")
        return makeDegenerate(cols, rows, threshold)
      }
    }
    val result = mask.map { case (ij, b) =>
      def get(ij:Cell) = mask.getOption(ij).getOrElse(false)
      val left = get(ij |- 1)
      val right = get(ij |+ 1)
      val up = get(ij -| 1)
      val down = get(ij +| 1)
      b && (left || right || up || down)
    }
    Blob(result, poly, result.flatten.getSpan)
  }



  private def makeDegenerate(cols:Int, rows:Int, threshold:Int):Blob[Boolean] = {
    var count = 0
    val poly = Polygon.fromLines3(
      Line(Point2d(0,0), Point2d(0, cols)),
      Line(Point2d(0,cols), Point2d(rows, cols)),
      Line(Point2d(rows,cols), Point2d(rows, 0)),
      Vector(Line(Point2d(rows,0), Point2d(0, 0)))
    )
    val mask = Array2d.tabulate(cols, rows) { case (i, j) =>
      val res = count < threshold
      count += 1
      res
    }
    Blob(mask, poly, mask.flatten.getSpan)
  }
}

case class Blob[T](mask:Array2d[T], poly:Polygon, span:Recti)
