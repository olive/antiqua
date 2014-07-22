package in.dogue.antiqua.graphics

import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.data.Array2d
import in.dogue.antiqua.Antiqua
import Antiqua._

object TileRenderer {
  def create(cols:Int, rows:Int)  = TileRenderer(cols, rows, Map(), Seq(), (0,0))
}
/** Applies a function to all on-screen tiles */
case class Filter(f:Cell => (Tile => Tile), origin:(Int,Int))

case class TileRenderer(cols:Int, rows:Int, private val draws:Map[Cell, Tile], filters:Seq[Filter], origin:(Int,Int)) {
  def move(i:Int, j:Int) = copy(origin = origin |+| ((i, j)))
  def movet(ij:(Int,Int)) = move(ij._1, ij._2)
  def project(rect:Recti) = Recti(origin.x, origin.y, 0, 0) + rect
  def withFilter(f:Cell => (Tile => Tile))(g:TileRenderer => TileRenderer) = {
    val filter = Filter(f, origin)
    val tr = copy(filters = filters :+ filter)
    this <*< g(tr)
  }

  def withMove(i:Int, j:Int)(f:TileRenderer => TileRenderer) = {
    move(i, j).<+<(f).move(-i, -j)
  }

  /**
   * Draws only the foreground of the given tile
   */
  def <|(ij:Cell, fg:Tile) = {
    val t = draws.get(ij |+| origin)
    t.map(tile => {
      this <+ (ij, tile.setFg(fg.fgColor).setCode(fg.code))
    }).getOrElse(this)
  }

  def <|?(opt:Option[(Cell, Tile)]) = {
    opt.foldLeft(this){_ <|~ _}
  }

  def <|~ :((Cell,Tile)) => TileRenderer = { case (p, f) =>
    <|(p, f)
  }

  def <||(s:TileGroup) = {
    s.foldLeft(this){ _ <|~ _}
  }

  /**
   * Draws only the foreground if there is already a tile in that place, else draws both foreground and background
   */
  def <+|(ij:Cell, fg:Tile) = {
    val t = draws.get(ij |+| origin)
    t.map(tile => {
      this <+ (ij, tile.setFg(fg.fgColor).setCode(fg.code))
    }).getOrElse(this <+ (ij, fg))
  }

  def <+|~ :((Cell,Tile)) => TileRenderer = { case (ij, f) =>
    <+|(ij, f)
  }

  def <++|(s:TileGroup) = {
    s.foldLeft(this){ _ <+|~ _}
  }

  /**
   * Applies the given function to the tile at (i, j). If no tile is
   * present, nothing happens.
   */
  def `$>`(ij:Cell, f:Tile => Tile):TileRenderer = {
    val t = draws.get(ij |+| origin)
    t.map(tile => {
      this <+ (ij, f(tile))
    }).getOrElse(this)
  }

  def `~$>`:((Cell,Tile=>Tile)) => TileRenderer = { case (p, f) =>
    `$>`(p, f)
  }

  def `$$>`(s:Seq[(Cell, Tile => Tile)]):TileRenderer = {
    s.foldLeft(this){ _ `~$>` _}
  }

  /**
   * Draw the given tile at (i, j)
   */
  def <+(pq:Cell, tile:Tile) = {
    val (i, j) = pq |+| origin
    if (i < 0 || i > cols - 1 || j < 0 || j > rows - 1) {
      this
    } else {
      val updated = draws.updated((i, j), tile)
      copy(draws = updated)
    }
  }

  def <+~ : ((Cell,Tile)) => TileRenderer = { case (ij, f) =>
    <+(ij, f)
  }

  def <+?(t:Option[(Cell,Tile)]): TileRenderer = {
    t.map {this <+~ _}.getOrElse(this)
  }

  def <++(draws:TileGroup): TileRenderer = {
    draws.foldLeft(this) { _ <+~ _ }
  }

  /** Apply a draw function to this renderer */
  def <+<(f:TileRenderer => TileRenderer): TileRenderer = {
    f(this)
  }

  def <+?<(f:Option[TileRenderer => TileRenderer]): TileRenderer = {
    f.foldLeft(this) { _ <+< _ }
  }

  def <++<(draws:Seq[TileRenderer => TileRenderer]): TileRenderer = {
    draws.foldLeft(this) { _ <+< _ }
  }

  /** Draw an animation group */
  def <##(draws:Seq[(Cell,Animation)]) = {
    draws.foldLeft(this) { case (tr, (p, a)) =>
      tr <+< a.draw(p)
    }
  }

  /**
   * Copies all draws from other to this, ignoring other's origin and filters
   */
  def <*<(other:TileRenderer) = {
    TileRenderer(cols, rows, draws ++ other.getDraws, Seq(), origin)
  }

  /** Clear the given renderer preserving its origin */
  def ^^^() = {
    TileRenderer(cols, rows, Map(), Seq(), origin)
  }

  def getDraws:Map[Cell,Tile] = {
    if (filters.length == 0) {
      draws
    } else {
      draws.par.map{ case (cell, t) =>
        val tt = filters.foldLeft(t) { case (tile, f) =>
          f.f(cell)(tile)
        }
        cell -> tt
      }.seq.toMap

    }
  }

  override def toString:String = {
    "TileRenderer@(%d,%d) draws(%d)" format (origin.x, origin.y, draws.count{case (_,_) => true})
  }
}
