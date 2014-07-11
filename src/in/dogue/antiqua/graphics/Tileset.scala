package in.dogue.antiqua.graphics

import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx.graphics.g2d.TextureRegion
import com.deweyvm.gleany.data.Recti
import com.deweyvm.gleany.AssetLoader
import in.dogue.antiqua.Implicits._
import in.dogue.antiqua.data.Array2d

case class Tileset(cols:Int, rows:Int, tileWidth:Int, tileHeight:Int, t:Texture) {
  val texture = TexturePacker.pack(t, cols*tileWidth, rows*tileHeight)
  private val regions = Array2d.tabulate(cols, rows) { case (i, j) =>
    makeRegion(i, j)
  }

  private def makeRegion(i:Int, j:Int) =
    AssetLoader.makeTextureRegion(texture, Recti(i * tileWidth, j * tileHeight, tileWidth, tileHeight).some)

  val blank = makeRegion(cols, rows)

  def getRegion(i:Int, j:Int):TextureRegion = {
    regions.get(i, j)
  }
}
