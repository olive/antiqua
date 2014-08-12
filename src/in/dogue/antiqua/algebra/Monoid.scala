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

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def zero: List[A] = List()

    override def add(a:List[A], b:List[A]) = a ++ b
  }

  implicit def seqMonoid[A]: Monoid[Seq[A]] = new Monoid[Seq[A]] {
    override def zero: Seq[A] = Seq()

    override def add(a:Seq[A], b:Seq[A]) = a ++ b
  }

  implicit def tupMonoid[A,B](implicit ev:Monoid[A], ev2:Monoid[B]):Monoid[(A,B)] = new Monoid[(A, B)] {
    override def zero: (A, B) = (ev.zero, ev2.zero)

    override def add(m1: (A, B), m2: (A, B)): (A, B) = (m1._1 <+> m2._1, m1._2 <+> m2._2)
  }

  implicit def mapMonoid[A,B](implicit ev:Monoid[B]):Monoid[Map[A, B]] = new Monoid[Map[A, B]] {
    override def zero = Map()
    override def add(m1:Map[A,B], m2:Map[A,B]) = {
      (for (k <- m1.keys ++ m2.keys) yield {
        val v1 = m1.getOrElse(k, ev.zero)
        val v2 = m2.getOrElse(k, ev.zero)
        k -> (v1 <+> v2)
      }).toMap.withDefaultValue(ev.zero)
    }
  }
}
