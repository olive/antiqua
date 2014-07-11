package in.dogue.antiqua.data

object Rot13 {
  def rot13(s: String) = s map { c =>
    val lower = c.toLower
    lower match {
      case ch if 'a' <= lower && lower <= 'm' => (c + 13).toChar
      case ch if 'n' <= lower && lower <= 'z' => (c - 13).toChar
      case ch => ch
    }

  }
}
