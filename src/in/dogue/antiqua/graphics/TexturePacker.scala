package in.dogue.antiqua.graphics

import com.deweyvm.gleany.graphics.{Screenshot, Color, Fbo}
import com.badlogic.gdx.graphics.g2d.{TextureRegion, SpriteBatch}
import com.badlogic.gdx.graphics.{OrthographicCamera, Pixmap, Texture}

object TexturePacker {
  private def makeBlank = {
    val p = new Pixmap(1, 1, Pixmap.Format.RGBA8888)
    p.drawPixel(0,0,Color.White.toLibgdxColor.toIntBits)
    val t = new Texture(p)
    p.dispose()
    t
  }

  def pack(tiles:Texture, width:Int, height:Int):Texture = {
    val blank = makeBlank
    val batch = new SpriteBatch()
    val cam = new OrthographicCamera(width, height)
    cam.setToOrtho(true, width, height)
    cam.update()
    val buf = new Fbo(width, height*2)
    buf.draw(() => {
      batch.begin()
      batch.setProjectionMatrix(cam.combined)
      batch.draw(tiles, 0, height/2, width, height/2)
      batch.draw(new TextureRegion(blank), 0, 0, 0, 0, 1, 1, width, height/2, 0)
      batch.end()
    })

    Screenshot.saveFrameBuffer(buf, "test.png")


    batch.dispose()
    buf.texture
  }

}

