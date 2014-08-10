package in.dogue.antiqua.data

class AugSeq[T](s:Seq[T]) {
  def pairwise:Seq[(T,T)] = {
    s match {
      case x0 +: xs0 =>
        def mkPairs(s:Seq[T]):Seq[(T,T)] = {
          s match {
            case x +: y +: xs => (x, y) +: mkPairs(y +: xs)
            case x +: xs => (x, x0) +: mkPairs(xs)
            case _ => Seq()
          }
        }
        mkPairs(s)
      case _ => Seq()
    }
  }


}
