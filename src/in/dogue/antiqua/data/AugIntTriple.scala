package in.dogue.antiqua.data

import in.dogue.antiqua.Antiqua.Vox

class AugIntTriple(p:Vox) {
  @inline def xy = (x, y)
  @inline def x = p._1
  @inline def y = p._2
  @inline def z = p._3
}
