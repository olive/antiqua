package in.dogue.antiqua.data.aug

class AugTuple2[A,B](t:(A,B)) {
  def map1[K](f:A => K) = t.copy(_1=f(t._1))
  def map2[K](f:B => K) = t.copy(_2=f(t._2))
  def @@[C](c:C) = (t._1, t._2, c)
}
