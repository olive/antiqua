package in.dogue.antiqua.ai

import in.dogue.antiqua.data.Graph
import scala.collection.mutable
import in.dogue.antiqua.Antiqua
import Antiqua._

object AStar {

  def pfindVox(start:Vox, end:Vox, gr:Graph[Vox,Vox]):Option[List[Vox]] = {
    def h(a:Vox, b:Vox) = {
      (a |-| b).mag2
    }
    pfind(start, end, gr, h)
  }

  def pfind[T,K](start:T, end:T, gr:Graph[T,K], h:(T,T)=>Double):Option[List[T]] = {
    val closed = mutable.Set[T]()

    val previous = mutable.Map[T,Option[T]]().withDefaultValue(None)

    val gmap = mutable.Map[T,Double]()
    gmap(start) = 0
    val fmap = mutable.Map[T,Double]()
    fmap(start) = gmap(start) + h(start, end)

    def diff(t:(T,Double), r:(T,Double)) = t._2 > r._2
    val open = mutable.PriorityQueue[(T,Double)]()(Ordering.fromLessThan(diff))
    val openSet = mutable.Set[T](start)
    open += start -> 0
    while (!open.isEmpty) {
      val current = open.dequeue()._1
      openSet -= current
      if (current == end) {
        return Dijkstra.rewind(start, end, previous)
      }
      closed += current
      for (n <- gr.getNeighbors(current) if !closed.contains(n)) {
        val ng = gmap(current) + h(current, n)
        val inOpen = openSet.contains(n)
        if (!inOpen || ng < gmap(n)) {
          previous(n) = current.some
          gmap(n) = ng
          val fscore =  ng + h(n, end)
          fmap(n) = fscore
          if (!inOpen) {
            open += n -> fscore
            openSet += n
          }
        }
      }
    }
    println("no path")
    return None
  }
}
