package in.dogue.antiqua.data.aug

class AugSet[T](s:Set[T]) {
  def takeAny:Option[(T, Set[T])] = {
    val first = s.headOption
    first map { f =>
      val rest = s - f
      (f, rest)
    }


  }
}
