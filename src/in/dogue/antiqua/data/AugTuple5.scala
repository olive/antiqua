package in.dogue.antiqua.data

class AugTuple5[A,B,C,D,E](t:(A, B, C, D, E)) {
  def map1[K](f:A => K) = t.copy(_1=f(t._1))
  def map2[K](f:B => K) = t.copy(_2=f(t._2))
  def map3[K](f:C => K) = t.copy(_3=f(t._3))
  def map4[K](f:D => K) = t.copy(_4=f(t._4))
  def map5[K](f:E => K) = t.copy(_5=f(t._5))
  def @@[F](c:F) = (t._1, t._2, t._3, t._4, t._5, c)
}
