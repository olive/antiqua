package in.dogue.antiqua.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua.Cell

object Animation {
  def create(frames:Vector[(Int,Tile)]) = {
    Animation(frames, 0, 0)
  }

  def singleton(t:Tile) = {
    Animation.create(Vector((1, t)))
  }

  def makeBlinker(speed:Int, tiles:Vector[Tile]) = {
    create((0 until tiles.length).map { (i:Int) => speed}.zip(tiles).toVector)
  }

}

case class Animation(frames:Vector[(Int,Tile)], ptr:Int, t:Int) {
  def update:Animation = {
    val (newT, newPtr) = if (t > frames(ptr)._1) {
      (0, (ptr + 1 + frames.length) % frames.length)
    } else {
      (t+1, ptr)
    }
    copy(t=newT, ptr=newPtr)
  }

  /** Draw the animation with the given foreground color rather than the Tile's color. */
  def drawWithFg(c:Color, ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <| (ij, getTile.setFg(c))
  }

  def getTile = frames(ptr)._2
  def getCode = getTile.code
  def getFg = getTile.fgColor

  def dimBg(amt:Double) = {
    val newFrames = frames map { case (i, t) =>
      (i, t.setBg(t.bgColor.dim(amt.toFloat)))
    }
    copy(frames=newFrames)
  }

  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+ (ij, getTile)
  }

  def drawFg(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <| (ij, getTile)
  }
}
