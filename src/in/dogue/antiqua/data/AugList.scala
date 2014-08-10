package in.dogue.antiqua.data

import in.dogue.antiqua.Antiqua
import Antiqua._

class AugList[T](t:List[T]) {
  def splitFind(f:T => Boolean) = {
    def inner(acc:List[T], t:List[T]):Option[(T, List[T])] = t match {
      case Nil => None
      case y :: ys =>
        if (f(y)) {
          Some (y @@ (acc ++ ys))
        } else {
          inner(y :: acc, ys)
        }
    }
    inner(List(), t)

  }



  def fold2[B](b:B, f:(T, B) => (T, B)):(List[T], B) = {
    t.foldLeft((List[T](), b)) { case ((acc, w), x) =>
      val (p, q) = f(x,w)
      (p :: acc) @@ q
    }
  }

  def fold3[B,C](b:B, c:C, f:(T, B, C) => (T, B, C)):(List[T], B, C) = {
    t.foldLeft((List[T](), b, c)) { case ((acc, w, y), x) =>
      val (p, q, r) = f(x,w,y)
      (p :: acc) @@ q @@ r
    }
  }
}
