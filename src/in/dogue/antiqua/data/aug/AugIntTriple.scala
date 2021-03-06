package in.dogue.antiqua.data.aug

import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Direction3

class AugIntTriple(p:Vox) {
  @inline def xy = (x, y)
  @inline def x = p._1
  @inline def y = p._2
  @inline def z = p._3
  @inline def |+|+|(other:Vox) = (x + other.x, y + other.y, z + other.z)
  @inline def |-|-|(other:Vox) = (x - other.x, y - other.y, z - other.z)
  @inline def mag = mag2.sqrt
  @inline def mag2 = x*x + y*y + z*z
  @inline def -->(d:Direction3) = p |+|+| d.dd
}
