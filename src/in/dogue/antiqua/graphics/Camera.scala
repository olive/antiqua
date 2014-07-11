package in.dogue.antiqua.graphics

import com.badlogic.gdx.graphics.OrthographicCamera

class Camera(width:Int, height:Int) {
  private val cam = {
    val c = new OrthographicCamera(1,1)
    c.setToOrtho(true, width, height)
    c
  }

  def translate(x:Int, y:Int) {
    cam.translate(x, y)
  }

  def zoom(f:Double) {
    cam.zoom = f.toFloat
  }

  def getProjection = {
    cam.update()
    cam.combined
  }
}
