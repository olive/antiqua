package in.dogue.antiqua.data.aug

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Array2d

class AugArray2dPair[A,B](a:Array2d[(A,B)]) {
  def unzip:(Array2d[A], Array2d[B]) = {
    a.map {case (_, p) => p._1} @@ a.map {case (_, p) => p._2}
  }
}
