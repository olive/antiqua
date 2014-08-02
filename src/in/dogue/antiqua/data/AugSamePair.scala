package in.dogue.antiqua.data

class AugSamePair[A](p:(A,A)) {
  def map[K](f:A => K) = (f(p._1), f(p._2))
}
