package in.dogue.antiqua.utils

import com.deweyvm.gleany.AssetLoader
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._

class TmxMap(name:String, layer:String) {
  private val tiles = AssetLoader.loadTmx(name).getTileLayer(layer)
  val rows = tiles.length
  val cols = tiles(0).length
  def get(ij:Cell) = tiles(ij.y)(ij.x) //[sic]
}
