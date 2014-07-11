package in.dogue.antiqua.data

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.Tile

class Code(val index:Int) extends AnyVal {
  def toCode = this

  def mkTile(bg:Color, fg:Color) =  {
    Tile(this, bg, fg)
  }
}

