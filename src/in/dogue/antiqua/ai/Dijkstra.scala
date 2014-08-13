package in.dogue.antiqua.ai

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import in.dogue.antiqua.Antiqua._
import in.dogue.antiqua.data.FiniteGraph

object Dijkstra {
  type Node = (Int,Int)

  private def diff(a:(Node, Double), b:(Node, Double)) = a._2 > b._2
  def rewind[T](start:T, end:T, previous:mutable.Map[T,Option[T]]):Option[List[T]] = {
    var u = end
    val result = ArrayBuffer[T]()
    result += u
    while(previous(u).isDefined) {
      val v = previous(u).get
      result += v
      u = v
      ()
    }
    result.reverse.toList.some
  }

  def pfind(start:Node, end:Node, g:FiniteGraph[Node,Node]):Option[List[Node]] = {
    def getNeighbors(c:Cell) = g.getNeighbors(c)
    val allNodes = g.getAll
    val dist:mutable.Map[Node, Double] = mutable.Map().withDefaultValue(Double.PositiveInfinity)
    val pq = mutable.PriorityQueue[(Node,Double)]()(Ordering.fromLessThan(diff))
    dist(start) = 0
    val previous:mutable.Map[Node,Option[Node]] = mutable.Map().withDefaultValue(None)
    for (n <- allNodes) {
      pq += (n -> dist(n))
    }

    while (!pq.isEmpty) {
      val u = pq.dequeue()._1
      if (u == end) {
        return rewind(start, end, previous)
      }
      for ((v,cost) <- getNeighbors(u) if !previous.contains(v)) {
        val alt = dist(u) + cost
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
