package in.dogue.antiqua.data

import in.dogue.antiqua.Implicits
import Implicits._

class AugInt(a:Int) {
  def %%(b:Int) = (a % b + b) % b
  def clamp(min:Int, max:Int) = {
    if (a < min) {
      min
    } else if (a > max) {
      max
    } else {
      a
    }
  }

  def drop1 = (a - 1).clamp(0, Int.MaxValue)
  def drop(i:Int) = (a - i).clamp(0, Int.MaxValue)
}
