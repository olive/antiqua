package in.dogue.antiqua.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.{Array2d, Code}
import in.dogue.antiqua.Antiqua.TileGroup
import com.deweyvm.gleany.AssetLoader
import in.dogue.antiqua.utils.TmxMap

object Tile {
  def makeGroup(cs:Vector[(Int,Int,Code,Color,Color)]):TileGroup = {
    cs.map { case (i, j, c, bg, fg) =>
      ((i, j), c.mkTile(bg, fg))
    }
  }

  def groupFromFile(name:String, layer:String, intToCode:Int => Code, codeToTile:Code => Tile):TileGroup = {
    val tiles = new TmxMap(name, layer)
    Array2d.tabulate(tiles.cols, tiles.rows) { case p =>
      codeToTile(intToCode(tiles.get(p)))
    }.flatten
  }
}

case class Tile(code:Code, bgColor:Color, fgColor:Color) {
  def setBg(c:Color) = copy(bgColor = c)
  def mapBg(f:Color => Color) = copy(bgColor = f(bgColor))
  def setFg(c:Color) = copy(fgColor = c)
  def mapFg(f:Color => Color) = copy(fgColor = f(fgColor))
  def setCode(c:Code) = copy(code=c)
}
