package in.dogue.antiqua.graphics

import in.dogue.antiqua.Antiqua._
import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.Code

object Rect {
  def createPlain(cols:Int, rows:Int, tile:Tile) = {
    val tiles = for (i <- 0 until cols; j <- 0 until rows) yield {
      ((i, j), tile)
    }
    Rect(cols, rows, tiles)
  }

  def createTextured(cols:Int, rows:Int, f:Random => Tile, r:Random) = {
    val tiles = for (i <- 0 until cols; j <- 0 until rows) yield {
      ((i, j), f(r))
    }
    Rect(cols, rows, tiles)
  }

  def createSimpleTextured(cols:Int, rows:Int, bg:Color, bgFactor:Int, fg:Color, fgFactor:Int, cds:Vector[Code], r:Random) = {
    def mkTile(r:Random) = {
      val bgColor = bg.dim(bgFactor + r.nextDouble)
      val fgColor = fg.dim(fgFactor + r.nextDouble)
      val code = cds.randomR(r)
      code.mkTile(bgColor, fgColor)
    }
    createTextured(cols, rows, mkTile, r)

  }
}

case class Rect(cols:Int, rows:Int, tiles:TileGroup) {
  def update = this
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <++ (tiles |++| ij)
  }

  def toTileGroup = tiles

  def filterDraw(ij:Cell)(f:Cell => Boolean)(tr:TileRenderer):TileRenderer = {
    tr <++ (tiles map { case (pq, t) =>
      if (f(pq)) {
        (ij |+| pq, t).some
      } else {
        None
      }
    }).flatten
  }
}

