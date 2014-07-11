package in.dogue.antiqua.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.Code

case class TileFactory(bg:Color, fg:Color) {
  def apply(c:Code) = c.mkTile(bg, fg)
}
