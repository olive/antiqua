package in.dogue.antiqua.data

import in.dogue.antiqua.Antiqua
import Antiqua._

class AugArray2dPair[A,B](a:Array2d[(A,B)]) {
  def unzip:(Array2d[A], Array2d[B]) = {
    a.map {case (_, p) => p._1} @@ a.map {case (_, p) => p._2}
  }
}
