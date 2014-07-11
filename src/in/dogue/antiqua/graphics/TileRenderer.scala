package in.dogue.antiqua.graphics

import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.data.Array2d
import in.dogue.antiqua.Implicits
import Implicits._

object TileRenderer {
  def create = TileRenderer(Map(), 0, 0)
}

case class TileRenderer(draws:Map[(Int,Int), Tile], originX:Int, originY:Int) {
  def move(i:Int, j:Int) = copy(originX = originX + i, originY = originY + j)
  def movet(ij:(Int,Int)) = move(ij._1, ij._2)
  def project(rect:Recti) = Recti(originX, originY, 0, 0) + rect


  def withMove(i:Int, j:Int)(f:TileRenderer => TileRenderer) = {
    move(i, j).<+<(f).move(-i, -j)
  }

  /**
   * Draws only the foreground of the given tile
   */
  def <|(i:Int, j:Int, fg:Tile) = {
    val t = draws.get((i + originX, j + originY))
    t.map(tile => {
      this <+ (i, j, tile.setFg(fg.fgColor).setCode(fg.code))
    }).getOrElse(this)
  }

  def <|~(t:(Int,Int,Tile)):TileRenderer = {
    val i = t._1
    val j = t._2
    val f = t._3
    <|(i, j, f)
  }

  def <||(s:Seq[(Int,Int,Tile)]) = {
    s.foldLeft(this){ _ <|~ _}
  }

  def `$>`(i:Int, j:Int, f:Tile => Tile):TileRenderer = {
    val t = draws.get((i + originX, j + originY))
    t.map(tile => {
      this <+ (i, j, f(tile))
    }).getOrElse(this)
  }

  def `~$>`(t:(Int,Int,Tile=>Tile)):TileRenderer = {
    val i = t._1
    val j = t._2
    val f = t._3
    `$>`(i, j, f)
  }

  def `$$>`(s:Seq[(Int, Int, Tile => Tile)]):TileRenderer = {
    s.foldLeft(this){ _ `~$>` _}
  }

  def <+(i:Int, j:Int, tile:Tile) = {
    val updated = draws.updated((i + originX, j + originY), tile)
    copy(draws = updated)
  }

  def <+~(t:(Int,Int,Tile)): TileRenderer = {
    val i = t._1
    val j = t._2
    val f = t._3
    <+(i, j, f)
  }

  def <+?(t:Option[(Int,Int,Tile)]): TileRenderer = {
    t.map {this <+~ _}.getOrElse(this)
  }
  def <++(draws:Seq[(Int,Int,Tile)]): TileRenderer = {
    draws.foldLeft(this) { _ <+~ _ }
  }
  def <+++(draws:Array2d[Tile]):TileRenderer = {
    draws.foldLeft(this){ _ <+~ _ }
  }

  def <+++<[T](draws:Array2d[T], f:T => Tile):TileRenderer = {
    <+++(draws.map { case (i, j, t) => f(t)})
  }


  def <+<(f:TileRenderer => TileRenderer): TileRenderer = {
    f(this)
  }

  def <+?<(f:Option[TileRenderer => TileRenderer]): TileRenderer = {
    f.foldLeft(this) { _ <+< _ }
  }

  def <++<(draws:Seq[TileRenderer => TileRenderer]): TileRenderer = {
    draws.foldLeft(this) { _ <+< _ }
  }

  def <#(i:Int, j:Int, a:Animation): (TileRenderer) => TileRenderer = {
    a.draw(i, j)
  }

  def <#~(t:(Int,Int,Animation)) = {
    val i = t._1
    val j = t._2
    val f = t._3
    <#(i, j, f)
  }

  def <##(draws:Seq[(Int,Int, Animation)]) = {
    draws.foldLeft(this) { case (tr, (i, j, a)) =>
      tr <+< a.draw(i, j)
    }
  }

  /**
   * Copies all draws from other to this, ignoring other's origin
   */
  def <*<(other:TileRenderer) = {
    TileRenderer(draws ++ other.draws, originX, originY)
  }

  def ^^^() = {
    TileRenderer(Map(), originX, originY)
  }

  override def toString:String = {
    "TileRenderer@(%d,%d) draws(%d)" format (originX, originY, draws.count{case (_,_) => true})
  }
}
