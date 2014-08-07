package in.dogue.antiqua.data

class AugDouble(a:Double) {
  def %%(b: Double) = (a % b + b) % b
}
