package in.dogue.antiqua.data

import scala.annotation.tailrec
import in.dogue.antiqua.Antiqua
import Antiqua._

object Graph {
  def flood[C,N](g:Graph[C,N], seed:C):Set[C] = {
    var set = Set[C]() + seed
    var filled = Set[C]()
    while (!set.isEmpty) {
      val Some((pos, newSet)) = set.takeAny
      filled = filled + pos
      set = newSet
      val ns = g.getNeighbors(pos)
      ns foreach { case (ppos,_) =>
        if (!filled.contains(ppos)) {
          set = set + ppos
        }

      }
    }
    filled
  }

  def floodAll[C, N](g:Graph[C, N], all:Set[C]) = {
    floodAllHelper(g, all, Seq())

  }

  @tailrec
  final def floodAllHelper[C, N](g:Graph[C, N], open:Set[C], sofar:Seq[Set[C]]):Seq[Set[C]] = {
    open.takeAny match {
      case Some((seed, rest)) =>
        val filled = flood(g, seed)
        val newOpen = open -- filled
        floodAllHelper(g, newOpen, filled +: sofar)
      case None => sofar
    }
  }
}

trait Graph[TCoord, TNode] {
  type Cost = Double
  def getNeighbors(c:TCoord):Seq[(TCoord,Cost)]
  def get(c:TCoord):TNode
}


trait FiniteGraph[TCoord, TNode] extends Graph[TCoord, TNode] {
  def getAll:Seq[TCoord]
}

