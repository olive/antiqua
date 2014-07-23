package in.dogue.antiqua.data

class AugAny[T](t: => T) {
  @inline
  def some:Option[T] = Some(t)
  @inline
  def onlyIf(b:Boolean):Option[T] = if (b) Some(t) else None
  @inline
  def dup = (t, t)

  def @@[B](b:B) = (t, b)
}
