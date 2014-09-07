package in.dogue.antiqua.data

import scala.collection.mutable.ArrayBuffer

object Flood {
  def flood[C,N](gr:Graph[C,N], c:C):Set[C] = floodRaw(gr, c)._1

  def floodFind[C,N](gr:Graph[C,N], c:C):C = floodRaw(gr, c)._2

  def floodRaw[C,N](gr:Graph[C, N], c:C):(Set[C], C) = {
    val work = collection.mutable.Queue(c)
    val done = collection.mutable.Set[C]()
    var last = c
    while (!work.isEmpty) {
      val next = work.dequeue()
      done += c
      last = next
      for (n <- gr.getNeighbors(next)) {
        work += n._1
      }
    }
    (done.toSet, last)
  }

  def floodAll[C,N](gr:Graph[C,N], all:Set[C]):List[Set[C]] = {
    var mall = all
    val filled = ArrayBuffer[Set[C]]()
    while (!mall.isEmpty) {
      val set = flood(gr, mall.head)
      mall = mall diff set
    }
    filled.toList
  }
}
