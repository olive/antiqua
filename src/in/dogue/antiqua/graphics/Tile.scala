package in.dogue.antiqua.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.Code

case class Tile(code:Code, bgColor:Color, fgColor:Color) {
  def setBg(c:Color) = copy(bgColor = c)
  def setFg(c:Color) = copy(fgColor = c)
  def setCode(c:Code) = copy(code=c)
}
