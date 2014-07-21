package in.dogue.antiqua.data

object Direction {
  case object Up extends Direction {
    override val dy = -1
    override val isVertical:Boolean = true
    override val opposite = Down
  }
  case object Down extends Direction {
    override val dy = 1
    override val isVertical:Boolean = true
    override val opposite = Up
  }
  case object Left extends Direction {
    override val dx = -1
    override val isHorizontal = true
    override val opposite = Right
  }
  case object Right extends Direction {
    override val dx = 1
    override val isHorizontal = true
    override val opposite = Left
  }
  val All = Vector(Up, Down, Left, Right)
}

sealed trait Direction {
  val dx:Int = 0
  val dy:Int = 0
  val isVertical:Boolean = false
  val isHorizontal:Boolean = false
  val opposite:Direction
}

