package in.dogue.antiqua.graphics

import in.dogue.antiqua.Implicits._
import scala.util.Random

object Rect {
  def createPlain(cols:Int, rows:Int, tile:Tile) = {
    val tiles = for (i <- 0 until cols; j <- 0 until rows) yield {
      (i, j, tile)
    }
    Rect(cols, rows, tiles)
  }

  def createTextured(cols:Int, rows:Int, f:Random => Tile, r:Random) = {
    val tiles = for (i <- 0 until cols; j <- 0 until rows) yield {
      (i, j, f(r))
    }
    Rect(cols, rows, tiles)
  }
}

case class Rect(cols:Int, rows:Int, tiles:Seq[(Int, Int, Tile)]) {
  def update = this
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <++ (tiles map { case (ii, jj, t) => (i+ii, j+jj, t)})
  }

  def filterDraw(i:Int, j:Int, f:(Int,Int) => Boolean)(tr:TileRenderer):TileRenderer = {
    tr <++ (tiles map { case (ii, jj, t) =>
      if (f(ii, jj)) {
        (i+ii, j+jj, t).some
      } else {
        None
      }
    }).flatten
  }
}

