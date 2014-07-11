package in.dogue.antiqua.graphics

import com.badlogic.gdx.graphics.{GL10, Texture}
import com.deweyvm.gleany.graphics.Color
import com.deweyvm.gleany.data.Point2d
import com.badlogic.gdx.graphics.g2d.{Sprite, TextureRegion, SpriteBatch}
import com.badlogic.gdx.Gdx
import scala.collection.mutable.ArrayBuffer
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType
import in.dogue.antiqua.data.Code

case class OglSprite(t:TextureRegion, color:Color) {
  def draw(batch:SpriteBatch, x:Int, y:Int) {
    batch.setColor(color.toLibgdxColor)
    batch.draw(t, x, y)
  }
}

class OglTile(tileset:Tileset) {
  val rows = tileset.rows
  val width = tileset.tileWidth
  val height = tileset.tileHeight
  val texture = tileset.t

  private def getRegion(index:Int) = {
    val x = index % rows
    val y = index / rows
    tileset.getRegion(x, y)
  }

  private def makeSprite(index:Int, color:Color, texture:Texture) = {
    val region = getRegion(index)
    OglSprite(region, color)
  }

  def getSprites(code:Code, fgColor:Color, bgColor:Color) = {
    val fg = makeSprite(code.index, fgColor, texture)
    val bg = OglSprite(tileset.blank, bgColor)
    (fg, bg)
  }
}



class Renderer(screenWidth:Int, screenHeight:Int, zoom:Int, tileset:Tileset){

  private val width = tileset.tileWidth
  private val height = tileset.tileHeight
  private val oglTile = new OglTile(tileset)

  val batch = new SpriteBatch
  val shape = new ShapeRenderer
  val camera = new Camera(screenWidth/zoom, screenHeight/zoom)

  private val spriteDraws = ArrayBuffer[() => Unit]()
  private val shapeDraws = ArrayBuffer[() => Unit]()

  def drawSprite(s:Sprite, x:Int, y:Int) {
    spriteDraws.append(() => {
      s.setPosition(x, y)
      s.draw(batch)
    })
  }

  def drawOglSprite(s:OglSprite, x:Int, y:Int) {
    spriteDraws.append(() => {
      s.draw(batch, x, y)
    })
  }

  def drawLine(pt:Point2d, pr:Point2d, color:Color) {
    shape.begin(ShapeType.Line)
    shape.setColor(color.toLibgdxColor)
    shape.line(pt.x.toFloat, pt.y.toFloat, pr.x.toFloat, pr.y.toFloat)
    shape.end()

  }

  def drawPoint(pt:Point2d, color:Color, size:Int=2) {
    shape.begin(ShapeType.Filled)
    shape.setColor(color.toLibgdxColor)
    shape.circle(pt.x.toFloat, pt.y.toFloat, size)
    shape.end()
  }

  def drawPolygonFloat(pts:Array[Float], color:Color) {
    shape.begin(ShapeType.Line)
    shape.setColor(color.toLibgdxColor)
    shape.polygon(pts)
    shape.end()
  }

  /*def drawPolygon(poly:Polygon, color:Color) {
    poly.lines foreach { line =>
      drawLine(line.p, line.q, color)
    }
  }*/

  def drawRect(x:Int, y:Int, rwidth:Int, rheight:Int, color:Color) {
    shapeDraws.append(() => {
      shape.begin(ShapeType.Line)
      shape.setColor(color.toLibgdxColor)
      shape.rect(x*width, y*height, rwidth*width, rheight*height)
      shape.end()
    })
  }

  def translateShape(x:Int, y:Int)(f:() => Unit) {
    camera.translate(-x, -y)
    shape.setProjectionMatrix(camera.getProjection)
    f()
    camera.translate(x, y)
  }

  def drawTileRaw(t:Tile, x:Double, y:Double) {
    val (fg, bg) = oglTile.getSprites(t.code, t.fgColor, t.bgColor)
    drawOglSprite(bg, x.toInt, y.toInt)
    drawOglSprite(fg, x.toInt, y.toInt)
  }


  def render(r:TileRenderer):TileRenderer = {
    Gdx.gl.glClearColor(0,0,0,1)
    Gdx.gl.glClear(GL10.GL_DEPTH_BUFFER_BIT | GL10.GL_COLOR_BUFFER_BIT)
    batch.begin()
    batch.setProjectionMatrix(camera.getProjection)
    r.draws.foreach {case((i, j), t) =>
      drawTileRaw(t, i*width, j*height)
    }
    spriteDraws foreach {_()}
    spriteDraws.clear()
    batch.end()
    shape.setProjectionMatrix(camera.getProjection)
    shapeDraws.foreach {_()}
    shapeDraws.clear()
    r
  }

}
