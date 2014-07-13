package in.dogue.antiqua.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.Code

object Tile {
  def makeGroup(cs:Vector[(Int,Int,Code,Color,Color)]):Seq[(Int,Int,Tile)] = {
    cs.map { case (i, j, c, bg, fg) =>
      (i, j, c.mkTile(bg, fg))
    }
  }
}

case class Tile(code:Code, bgColor:Color, fgColor:Color) {
  def setBg(c:Color) = copy(bgColor = c)
  def setFg(c:Color) = copy(fgColor = c)
  def setCode(c:Code) = copy(code=c)
}
