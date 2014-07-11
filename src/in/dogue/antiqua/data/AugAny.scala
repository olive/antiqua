package in.dogue.antiqua.data

class AugAny[T](t:T) {
  def some:Option[T] = Some(t)
}
