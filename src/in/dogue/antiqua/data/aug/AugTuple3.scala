package in.dogue.antiqua.data.aug

class AugTuple3[A,B,C](t:(A, B, C)) {
  def map1[K](f:A => K) = t.copy(_1=f(t._1))
  def map2[K](f:B => K) = t.copy(_2=f(t._2))
  def map3[K](f:C => K) = t.copy(_3=f(t._3))
  def @@[D](c:D) = (t._1, t._2, t._3, c)
}
