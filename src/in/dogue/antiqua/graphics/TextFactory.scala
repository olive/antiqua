package in.dogue.antiqua.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.Code
import in.dogue.antiqua.Antiqua
import Antiqua._

object TextFactory {
  def bw(unicodeToCode:Char => Code) = TextFactory(Color.Black, Color.White, unicodeToCode)
}
case class TextFactory(bgColor:Color, fgColor:Color, unicodeToCode:Char => Code) {
  def withBg(c:Color) = copy(bgColor = c)
  def withFg(c:Color) = copy(fgColor = c)

  private def makeTiles(s:Vector[Code], bgColor:Color, fgColor:Color) = {
    s.map{_.mkTile(bgColor, fgColor)}.toVector
  }

  def textLines(s:String) = {
    val lines = s.split('\n')
    lines.map(create)
  }

  def multiline(s:String):TileGroup = {
    val lines = s.split('\n')
    fromLines(lines)
  }

  def fromLines(s:Seq[String]):TileGroup = {
    val res = (s map create).zipWithIndex.map { case (line, k) =>
      line.toTileGroup |++| ((0, k))
    }.toSeq
    res.flatten
  }

  def create(s:String) = {
    val tiles = makeTiles(s.map(unicodeToCode).toVector, bgColor, fgColor)
    Text(tiles, this)
  }

  def fromCodes(s:Vector[Code]) = {
    val tiles = makeTiles(s, bgColor, fgColor)
    Text(tiles, this)
  }
}
