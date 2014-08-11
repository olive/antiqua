package in.dogue.antiqua.data

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.Antiqua
import Antiqua._

object Array2dView {
  def cut[T](arr:Array2d[T], x_ :Int, y_ :Int, width:Int, height:Int) = {
    val cols_ = math.min(width + x_, arr.cols)
    val rows_ = math.min(height + y_, arr.rows)
    new Array2dView[T] {
      val x = x_
      val y = y_
      val cols = cols_
      val rows = rows_
      protected val underlying = arr
    }
  }
}

trait Array2dView[T] {
  val x:Int
  val y:Int
  val cols:Int
  val rows:Int
  protected val underlying:Array2d[T]
  def get(ij:Cell) = underlying.get(ij |+| ((x, y)))
  def foldLeft[R](r:R)(f:(R, (Cell,T)) => R): R = {
    (for (i <- 0 until cols;j<-0 until rows; p = (i, j)) yield {
      (p, get(p))
    }).foldLeft(r)(f)
  }
}
