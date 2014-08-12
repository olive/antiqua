package in.dogue.antiqua.data

import in.dogue.antiqua.Antiqua
import Antiqua._

object Array3d {
  def tabulate[T](cols:Int, rows:Int, layers:Int)(f:Vox => T) = {
    val vs = Vector.tabulate(layers) { k =>
      Array2d.tabulate(cols, rows) { case (i, j) => f((i, j, k))}
    }
    Array3d(cols, rows, layers, vs)
  }
}

case class Array3d[T](cols:Int, rows:Int, layers:Int, vs:Vector[Array2d[T]]) {
  @inline private def cpy(v:Vector[Array2d[T]]) = Array3d(cols, rows, layers, v)
  def get(v:Vox):T = vs(v.z).get(v.xy)
  def getLayer(z:Int) = vs(z)
  def getOption(v:Vox):Option[T] = vs(v.z).getOption(v.xy) //fixme, not safe in z index
  def updated(v:Vox, t:T):Array3d[T] = cpy(vs.updated(v.z, vs(v.z).updated(v.xy, t)))
  def update(v:Vox, f:T=>T):Array3d[T]= cpy(vs.updated(v.z, vs(v.z).update(v.xy, f)))
  def flatten:Seq[(Vox,T)] = vs.zipWithIndex.map{ case (a, z) => a.flatten.map { case ((i, j), t) => ((i, j, z), t)}}.flatten

}
