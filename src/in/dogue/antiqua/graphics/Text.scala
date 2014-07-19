package in.dogue.antiqua.graphics

import in.dogue.antiqua.Antiqua
import Antiqua._

case class Text(tiles:Vector[Tile], f:TextFactory)  {
  val length = tiles.length
  def append(s:String) = {
    val other = f.create(s)
    Text(tiles ++ other.tiles, f)
  }

  def toTileGroup = tiles.zipWithIndex.map { case (t, i) => (i, 0, t)}
  def filterToTileGroup(f:Tile=>Boolean) = tiles.zipWithIndex.map { case (t, i) =>
    if (f(t)) {
      (i, 0, t).some
    } else {
      None
    }
  }.flatten
  def mapF(func:TextFactory=>TextFactory) = copy(f=func(f))

  def draw(i:Int, j:Int)(r:TileRenderer):TileRenderer = {
    drawSub(tiles.length)(i, j)(r)
  }

  def drawSub(index:Int)(i:Int, j:Int)(r:TileRenderer):TileRenderer = {
    r <++ tiles.take(index).zipWithIndex.map{case (t, k) => (i + k, j, t)}
  }

  def drawFg(i:Int, j:Int)(r:TileRenderer):TileRenderer = {
    drawFgSub(tiles.length)(i, j)(r)
  }

  def drawFgSub(index:Int)(i:Int, j:Int)(r:TileRenderer):TileRenderer = {
    r <++| tiles.take(index).zipWithIndex.map{case (t, k) => (i + k, j, t)}
  }

}
