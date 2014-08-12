package in.dogue.antiqua.data.aug

class AugDouble(a:Double) {
  def %%(b: Double) = (a % b + b) % b
}
