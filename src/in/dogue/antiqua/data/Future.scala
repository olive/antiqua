package in.dogue.antiqua.data

sealed trait FutureState[+T]
case object FutureComputing extends FutureState[Nothing]
case class FutureFinished[T](t:T) extends FutureState[T]
case class FutureError(e:Exception) extends FutureState[Nothing]
class Future[T](f:() => T) {
  private var value:Option[T] = None
  private var error:Option[Exception] = None
  def update:FutureState[T] = {
    this.synchronized {
      error.map(FutureError.apply).getOrElse(value.map(FutureFinished.apply).getOrElse(FutureComputing))

    }

  }

  private def dispatch() = {
    val rble = new Runnable() {
      def run = {
        try {
          val v = Some(f())
          this.synchronized {
            value = v
          }
        } catch {
          case e:Exception =>
            val s = Some(e)
            this.synchronized {
              error = s
            }

        }
      }
    }
    new Thread(rble).start()
  }

  dispatch()
}
