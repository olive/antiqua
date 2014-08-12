package in.dogue.antiqua.data.aug

import in.dogue.antiqua.Antiqua
import Antiqua._

class AugInt(a:Int) {
  def %%(b:Int) = (a % b + b) % b
  @inline def clamp(min:Int, max:Int) = {
    if (a < min) {
      min
    } else if (a > max) {
      max
    } else {
      a
    }
  }

  @inline def drop1 = (a - 1).clamp(0, Int.MaxValue)
  @inline def drop(i:Int) = (a - i).clamp(0, Int.MaxValue)
}
