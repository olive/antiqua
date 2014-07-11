package in.dogue.antiqua.data

class AugBool(b:Boolean) {
  def select[T](f:T, t:T) = if (b) t else f
}
