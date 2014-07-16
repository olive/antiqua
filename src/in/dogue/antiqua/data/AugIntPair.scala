package in.dogue.antiqua.data

class AugIntPair(p:(Int,Int)) {
  @inline def x = p._1
  @inline def y = p._2
  def |+|(pr:(Int,Int)) = (p._1 + pr._1, p._2 + pr._2)
  def |+(i:Int) = (p._1 + i, p._2)
  def +|(j:Int) = (p._1, p._2 + j)

  def |-|(pr:(Int,Int)) = (p._1 - pr._1, p._2 - pr._2)
  def |-(i:Int) = (p._1 - i, p._2)
  def -|(j:Int) = (p._1, p._2 - j)

  def -->(d:Direction) = (p._1 + d.dx, p._2 + d.dy)
}
