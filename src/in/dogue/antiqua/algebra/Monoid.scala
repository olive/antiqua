package in.dogue.antiqua.algebra

import in.dogue.antiqua.Antiqua
import Antiqua._

trait Monoid[M] {
  def zero:M
  def add(m1:M, m2:M):M

  class MonoidOps(m:M) {
    def <+>(o:M):M = add(m, o)
  }

}


object Monoid {
  implicit def mkMonoidOps[T](lhs:T)(implicit ev:Monoid[T])  = new ev.MonoidOps(lhs)

  implicit object StringMonoid extends Monoid[String] {
    def zero = ""
    def add(s1:String, s2:String) = s1 + s2
  }

  implicit object IntMonoid extends Monoid[Int] {
    def zero = 0
    def add(i:Int, j:Int) = i + j
  }

  implicit object ListMonoid extends Monoid[List[SomeT]] {
    def zero = List()
    def add(a:List[SomeT], b:List[SomeT]) = a ++ b
  }
  type SomeT = T forSome {type T}


  implicit def seqMonoid[A]: Monoid[Seq[A]] = new Monoid[Seq[A]] {
    override def zero: Seq[A] = Seq()

    override def add(a:Seq[A], b:Seq[A]) = a ++ b
  }

  implicit def tupMonoid[A,B](implicit ev:Monoid[A], ev2:Monoid[B]):Monoid[(A,B)] = new Monoid[(A, B)] {
    override def zero: (A, B) = (ev.zero, ev2.zero)

    override def add(m1: (A, B), m2: (A, B)): (A, B) = (m1._1 <+> m2._1, m1._2 <+> m2._2)
  }

}
