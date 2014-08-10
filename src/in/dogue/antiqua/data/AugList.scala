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
}
