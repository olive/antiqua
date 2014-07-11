package in.dogue.antiqua.graphics

case class Text(tiles:Vector[Tile], f:TextFactory)  {
  val length = tiles.length
  def append(s:String) = {
    val other = f.create(s)
    Text(tiles ++ other.tiles, f)
  }

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
    r <|| tiles.take(index).zipWithIndex.map{case (t, k) => (i + k, j, t)}
  }
}
