package in.dogue.antiqua

import in.dogue.antiqua.data._
import in.dogue.antiqua.graphics.{Animation, Tile}
import in.dogue.antiqua.algebra.Monoid

object Antiqua {
  type TileGroup = Seq[(Cell,Tile)]
  type AnimationGroup = Seq[(Cell,Animation)]
  type Cell = (Int,Int)
  implicit def any2Aug[A](a: => A) = new AugAny(a)
  implicit def opt2Aug[A](o:Option[A]) = new AugOption(o)
  implicit def bool2Aug(b:Boolean) = new AugBool(b)
  implicit def indexedSeq2Aug[A](s:IndexedSeq[A]) = new AugIndexedSeq(s)
  implicit def indexedSeq2AugProp[A](s:IndexedSeq[(Int, A)]) = new AugIndexedProb(s)
  implicit def num2Aug[A](a:A)(implicit n: Numeric[A]) = new AugNum(a)
  implicit def seq2AugCellSeq[A](seq:Seq[(Cell,A)]) = new AugCellSeq(seq)
  implicit def intTup2Aug[A](tup:(Int,Int)) = new AugIntPair(tup)
  implicit def int2Aug(i:Int) = new AugInt(i)
  implicit def cp437_2Code(cp:CP437) = new Code(cp.index)
  @inline def id[T](t:T) = t

  def impossible = throw new Exception("Impossible")


}
