package in.dogue.antiqua.data

class AugOption[T](o:Option[T]) {
  def <|>(p:Option[T]) = o match {
    case x@Some(_) => x
    case None => p
  }
}
