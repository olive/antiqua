package in.dogue.antiqua.data.aug

import in.dogue.antiqua.Antiqua._

class AugVector[T](v:Vector[T]) {
  def getOption(i:Int):Option[T] = if (i < 0 || i > v.length - 1) None else v(i).some
}
