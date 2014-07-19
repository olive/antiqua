package in.dogue.antiqua.data

class Future[T](f:() => T) {
  var done = false
  var value:Option[T] = None
  def update:Option[T] = {
    value
  }

  private def dispatch() = {
    val rble = new Runnable() {
      def run = {
        val v = f()
        value = Some(v)
      }
    }
    new Thread(rble).start()
  }

  dispatch()
}
