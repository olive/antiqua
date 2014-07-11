package in.dogue.antiqua.ui

import in.dogue.antiqua.graphics._
import in.dogue.antiqua.graphics.Text
import in.dogue.antiqua.data.Code

object Line {
  def create(v:Text, sound:() => Unit, isBlank:Code=>Boolean) = Line(v, sound, isBlank, 0, 0)
}
case class Line(v:Text, sound: () => Unit, isBlank:Code=>Boolean, ptr:Int, t:Int) {
  val speed = 2
  def isFinished = ptr >= v.length
  def update = {
    val (newT, newPtr) = if (t > speed) {
      if (ptr < v.length && isBlank(v.tiles(ptr).code)) {
        sound()
      }
      (0, ptr+1)
    } else {
      (t+1, ptr)
    }
    copy(ptr=Math.min(v.length, newPtr), t=newT)
  }
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< v.drawFgSub(ptr)(i, j)
  }
}


case class TextBox(lines:Vector[Line], ptr:Int) {

  def atEnd = ptr >= lines.length
  private def updateLast() = {
    lines.updated(ptr, lines(ptr).update)
  }
  def update:TextBox = {
    if (!atEnd && lines(ptr).isFinished) {
      copy(ptr=ptr+1)
    } else if (ptr < lines.length) {
      copy(lines=updateLast())
    } else {
      this
    }
  }
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    val bound = Math.min(lines.length, ptr + 1)
    tr <++< (for (k <- 0 until bound) yield {
      lines(k).draw(i, j+k) _
    })
  }

  def isFinished:Boolean = atEnd && lines(ptr-1).isFinished
}



