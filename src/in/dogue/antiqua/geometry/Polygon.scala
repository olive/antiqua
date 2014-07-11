package in.dogue.antiqua.geometry

import com.deweyvm.gleany.data.Point2d
import in.dogue.antiqua.Implicits
import Implicits._

object Polygon {
  def fromLines(lines:Vector[Line]) = lines match {
    case a +: b +: c +: rest =>
      new Polygon(lines).some
    case _ => None
  }

  def fromLines3(l0:Line, l1:Line, l2:Line, rest:Vector[Line]) = {
    new Polygon(l0 +: l1 +: l2 +: rest)
  }

  private var count = BigInt(0)
  def fromPoints(points:Vector[Point2d]):Option[Polygon] = {
    val toPair = points(points.length - 1) +: points
    val lines = (0 until toPair.length - 1).map { i =>
      val p1 = toPair(i)
      val p2 = toPair(i+1)
      Line(p1, p2)
    }
    fromLines(lines.toVector)
  }



  def filterDuplicates(ps:Vector[Polygon]) = {
    def polyClose(p1:Polygon, p2:Polygon) = {
      math.abs((p1.centroid - p2.centroid).magnitude2) < 0.01
    }
    ps.foldLeft(Vector[Polygon]()) { case (acc, p) =>
      if (!acc.exists{polyClose(_, p)}) {
        p +: acc
      } else {
        acc
      }
    }
  }

}

class Polygon private (val lines:Vector[Line]) {

  val code = Polygon.count
  Polygon.count += 1
  lazy val points:Vector[Point2d] = {
    val pts = lines map {_.p}
    pts :+ pts(0)
  }
  lazy val signedArea:Double = {
    var sum = 0.0
    for (i <- 0 until points.length - 1) {
      val xi0 = points(i).x
      val xi1 = points(i+1).x
      val yi0 = points(i).y
      val yi1 = points(i+1).y
      sum +=  xi0*yi1 - xi1*yi0
    }
    sum/2
  }

  //this assumes that the lines are in cw or ccw order
  lazy val centroid:Point2d = {
    var cx = 0.0
    var cy = 0.0
    for (i <- 0 until points.length - 1) {
      val xi0 = points(i).x
      val xi1 = points(i+1).x
      val yi0 = points(i).y
      val yi1 = points(i+1).y
      val p = xi0*yi1 - xi1*yi0
      cx += (xi0 + xi1)*p
      cy += (yi0 + yi1)*p
    }
    Point2d(cx, cy)/(6*signedArea)
  }

  /**
   * the distance between the centroid and farthest point
   */
  lazy val radius:Double = {
    points.map { p => (p - centroid).magnitude2 }.max.sqrt
  }

  def upperLeft:Option[Point2d] = {
    val p1 = lines.map {l => Vector(l.p, l.q)}
    val points:Vector[Point2d] = p1.flatten

    points.sortBy(_.x).sortBy(_.y).headOption
  }

  def isAdjacent(other:Polygon):Boolean = {
    lines exists other.lines.contains
  }

  def contains(pt:Point2d):Boolean = {
    val ray = new Line(pt, Point2d(Int.MaxValue, 5000))
    val intersections = lines.foldLeft(0){case (acc, line) =>
      if (ray.intersectPointEnd(line).isDefined) {
        acc + 1
      } else {
        acc
      }
    }
    intersections.isOdd
  }

  def translate(p:Point2d) = new Polygon(lines.map {_.translate(p)})

  override def equals(obj:Any) = {
    if (!obj.isInstanceOf[Polygon]) {
      false
    } else {
      val other = obj.asInstanceOf[Polygon]
      other.code == code
    }
  }

  override def hashCode() = code.toInt
}
