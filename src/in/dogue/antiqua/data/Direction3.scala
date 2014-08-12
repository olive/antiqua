package in.dogue.antiqua.data

object Direction3 {
  val Planar = Vector(North, East, South, West)
  val Orthogonal = Vector(Upward, Downward)
  val All = Planar ++ Orthogonal
}

sealed trait Direction3 {
  val dx:Int
  val dy:Int
  val dz:Int
}
case object Upward extends Direction3 {
  val dx = 0
  val dy = 0
  val dz = 1
}
case object Downward extends Direction3 {
  val dx = 0
  val dy = 0
  val dz = -1
}
case object North extends Direction3 {
  val dx = 0
  val dy = -1
  val dz = 0
}
case object South extends Direction3 {
  val dx = 0
  val dy = 1
  val dz = 0
}
case object East extends Direction3 {
  val dx = -1
  val dy = 0
  val dz = 0
}
case object West extends Direction3 {
  val dx = 1
  val dy = 0
  val dz = 0
}
