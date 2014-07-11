package in.dogue.antiqua.data

class AugPosSeq[T](s:Seq[(Int,Int,T)]) {
  def smap(f:T => T) = s.map{ case (i, j, t) => (i, j, f(t))}
  def |+|(p:Int, q:Int) = s.map { case (i, j, t) => (i + p, j + q, t)}
}
