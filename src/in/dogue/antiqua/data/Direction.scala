package in.dogue.antiqua.data

object Direction {
  case object Up extends Direction {
    override val dy = -1
    override val isVertical:Boolean = true
  }
  case object Down extends Direction {
    override val dy = 1
    override val isVertical:Boolean = true
  }
  case object Left extends Direction {
    override val dx = -1
    override val isHorizontal = true
  }
  case object Right extends Direction {
    override val dx = 1
    override val isHorizontal = true
  }

}

sealed trait Direction {
  val dx:Int = 0
  val dy:Int = 0
  val isVertical:Boolean = false
  val isHorizontal:Boolean = false
}

