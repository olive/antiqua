package in.dogue.antiqua.data

class AugAny[T](t: => T) {
  @inline
  def some:Option[T] = Some(t)
  @inline
  def onlyIf(b:Boolean):Option[T] = if (b) Some(t) else None

  @inline
  def onlyIfs(b:Boolean):Seq[T] = if(b) Seq(t) else Seq()


  @inline
  def dup = (t, t)

  @inline
  def @@[B](b:B) = (t, b)

  @inline
  def seq = Seq(t)

  def doo[K](f:T=>K) = f(t)
}
