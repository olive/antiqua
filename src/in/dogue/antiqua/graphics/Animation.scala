package in.dogue.antiqua.graphics

import com.deweyvm.gleany.graphics.Color

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

  def drawWithFg(c:Color, i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <| (i, j, getTile.setFg(c))
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

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+ (i, j, getTile)
  }
}
