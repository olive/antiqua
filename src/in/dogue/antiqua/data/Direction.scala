package in.dogue.antiqua.data

object Direction {
  case object Up extends Direction {
    override val dy = -1
  }
  case object Down extends Direction {
    override val dy = 1
  }
  case object Left extends Direction {
    override val dx = -1
  }
  case object Right extends Direction {
    override val dx = 1
  }

}

sealed trait Direction {
  val dx:Int = 0
  val dy:Int = 0
}

