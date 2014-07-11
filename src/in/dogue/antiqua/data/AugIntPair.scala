package in.dogue.antiqua.data

class AugIntPair(p:(Int,Int)) {
  def x = p._1
  def y = p._2
  def +(q:(Int,Int)) = (p._1 + q._1, p._2 + q._2)
  def -->(d:Direction) = (p._1 + d.dx, p._2 + d.dy)
}
