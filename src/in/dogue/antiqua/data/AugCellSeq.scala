package in.dogue.antiqua.data

import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua._

class AugCellSeq[T](s:Seq[(Cell, T)]) {

  def smap(f:T => T) = s.map{ case (p, t) => (p, f(t))}
  def |++|(pq:(Int,Int)) = s.map { case (ij, t) => (ij |+| pq, t)}
  def ++|(q:Int) = s.map { case ((i, j), t) => ((i, j + q), t)}
  def |++(p:Int) = s.map { case ((i, j), t) => ((i + p, j), t)}
  def sfilter(f:T => Boolean) = s.filter { case ((i, j), t) => f(t)}
  def getSpan = {
    var mini = Int.MaxValue
    var maxi = 0
    var minj = Int.MaxValue
    var maxj = 0
    for (((i, j), t) <- s) {
      mini = Math.min(i, mini)
      maxi = Math.max(i, maxi)
      minj = Math.min(j, minj)
      maxj = Math.max(j, maxj)
    }
    Recti(mini,minj, maxi - mini + 1, maxj - minj + 1)
  }

}
