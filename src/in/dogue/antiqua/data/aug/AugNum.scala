package in.dogue.antiqua.data.aug

class AugNum[T](rep:T)(implicit n: Numeric[T]) {
  def clamp(min: T, max: T):T = {
    if (n.lt(rep, min)) {
      min
    } else if (n.gt(rep, max)) {
      max
    } else {
      rep
    }
  }

  def max(other:T) = n.max(rep, other)
  def min(other:T) = n.min(rep, other)
  def inRange(min:T, max:T) = {
    val dmin = n.toDouble(min)
    val dmax = n.toDouble(max)
    val d = n.toDouble(rep)
    d >= dmin && d < dmax
  }
  def isEven:Boolean = n.toInt(rep) % 2 == 0
  def isOdd:Boolean = n.toInt(rep) % 2 == 1

  def sqrt:Double = math.sqrt(n.toDouble(rep))
  def sq:Double = {
    val x = n.toDouble(rep)
    x*x
  }
}
