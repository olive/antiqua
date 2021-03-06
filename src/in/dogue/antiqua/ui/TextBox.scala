package in.dogue.antiqua.ui

import in.dogue.antiqua.graphics._
import in.dogue.antiqua.graphics.Text
import in.dogue.antiqua.data.Code
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._

object TextLine {
  def create(v:Text, sound:() => Unit, isBlank:Code=>Boolean) = TextLine(v, sound, isBlank, 0, 0)
}
case class TextLine(v:Text, sound: () => Unit, isBlank:Code=>Boolean, ptr:Int, t:Int) {
  def reset = copy(t=0, ptr=0)
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
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    tr <+< v.drawFgSub(ptr)(ij)
  }
}


object TextBox {
  def create(lines:Vector[TextLine]) = TextBox(lines, 0)
}

case class TextBox private (lines:Vector[TextLine], ptr:Int) {
  def reset = copy(lines=lines.map{_.reset}, ptr=0)
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
  def draw(ij:Cell)(tr:TileRenderer):TileRenderer = {
    val bound = Math.min(lines.length, ptr + 1)
    tr <++< (for (k <- 0 until bound) yield {
      lines(k).draw(ij +| k) _
    })
  }

  def isFinished:Boolean = atEnd && lines(ptr-1).isFinished
}



