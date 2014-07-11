package in.dogue.antiqua.data

import scala.util.Random

class AugIndexedSeq[T](s:IndexedSeq[T]) {
  def randomR(r:Random) = s(r.nextInt(s.length))
}

class AugIndexedProb[T](s:IndexedSeq[(Int, T)]) {
  def expand = {
    (for ((k, t) <- s) yield {
      for (_ <- 0 until k) yield t
    }).flatten
  }
}
