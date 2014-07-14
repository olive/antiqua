package in.dogue.antiqua.ai

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import in.dogue.antiqua.Antiqua._

object Dijkstra {
  type Node = (Int,Int)

  private def diff(a:(Node, Int), b:(Node, Int)) = a._2 < b._2
  private def rewind(start:Node, end:Node, previous:mutable.Map[Node,Option[Node]]) = {
    var u = end
    val result = ArrayBuffer[Node]()
    result += u
    while(previous(u).isDefined) {
      val v = previous(u).get
      result += v
      u = v
      ()
    }
    result.some
  }

  /** warning untested */
  def pfind(start:Node, end:Node, getNeighbors:(Node) => Seq[Node], getAll: => Seq[Node]):Option[Seq[Node]] = {
    val allNodes = getAll
    val dist:mutable.Map[Node, Int] = mutable.Map().withDefaultValue(Int.MaxValue)
    val pq = mutable.PriorityQueue[(Node,Int)]()(Ordering.fromLessThan(diff))
    dist(start) = 0
    val previous:mutable.Map[Node,Option[Node]] = mutable.Map().withDefaultValue(None)
    for (n <- allNodes) {
      pq += (n -> dist(n))
    }

    while (!pq.isEmpty) {
      val u = pq.head._1
      if (u == end) {
        return rewind(start, end, previous)
      }
      for (v <- getNeighbors(u)) {
        val alt = dist(u) + 1//length(u,v)
        if (alt < dist(v)) {
          dist(v) = alt
          previous(v) = u.some
          pq += (v -> alt)
          ()
        }
      }
    }
    None

  }
}
