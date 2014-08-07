package in.dogue.antiqua.data

import in.dogue.antiqua.Antiqua._

object Array2d {
  def tabulate[T](cols:Int, rows:Int)(f:(Int,Int) => T):Array2d[T] = {
    val array = Vector.tabulate(cols*rows) { k =>
      val (i, j) = indexToCoords(k, cols)
      f(i, j)
    }
    new Array2d(array, cols, rows)
  }

  def parTabulate[T](cols:Int, rows:Int)(f:(Int,Int) => T):Array2d[T] = {
    val elts = (0 until cols*rows).par.map { k =>
      val (i, j) = indexToCoords(k, cols)
      f(i, j)
    }
    new Array2d(Vector(elts.seq:_*), cols, rows)
  }

  /**
   * the parameter t is evaluated once
   */
  def fill[T](cols:Int, rows:Int)(t:T):Array2d[T] = {
    new Array2d(Vector.fill(cols*rows)(t), cols, rows)
  }

  def unsafeGetElements[T](a:Array2d[T]):IndexedSeq[T] = a.elements

  def indexToCoords(k:Int, cols:Int):Cell = (k % cols, k / cols)
  def coordsToIndex(ij:Cell, cols:Int):Int = ij.x + ij.y*cols


}



class Array2d[T](private val elements:Vector[T],
                 val cols :Int,
                 val rows :Int) {
  outer =>
  import Array2d._

  def strictGetAll:IndexedSeq[T] = elements

  def count(f:(Cell,T)=>Boolean):Int = {
    elements.zipWithIndex.count {case (t, k) =>
      val ij = indexToCoords(k, cols)
      f(ij, t)
    }
  }

  def map[K](f:(Cell,T)=>K) = {
    Array2d.tabulate(cols, rows) { case ij =>
      val elt = get(ij)
      f(ij, elt)
    }
  }

  def foreach(f:(Cell,T) => Unit) {
    val _ = Array2d.tabulate(cols, rows) { case ij =>
      val elt = get(ij)
      f(ij, elt)
    }
    ()
  }

  def flatten:Seq[(Cell,T)] = {
    map[(Cell,T)]{case (p, z) => (p, z)}.elements

  }

  def update(ij:Cell, f:T => T) = {
    val k = coordsToIndex(ij, cols)
    getOption(ij).map {t =>
      new Array2d(elements.updated(k, f(t)), cols, rows)
    }.getOrElse{
      this
    }
  }

  def updated(ij:Cell, t:T) = {
    val k = coordsToIndex(ij, cols)
    new Array2d(elements.updated(k, t), cols, rows)
  }

  def groupBy[K](f:T => K): Map[K, Vector[T]] = elements.groupBy(f)

  def max[K >: T](implicit cmp: Ordering[K]):T = elements.max(cmp)
  def min[K >: T](implicit cmp: Ordering[K]):T = elements.min(cmp)

  def find(f:T=> Boolean):Option[(Cell, T)] = {
    elements.zipWithIndex.find {case (e, i) =>
      f(e)
    }.flatMap{case (e, i) =>
      val xy = indexToCoords(i, cols)
      (xy, e).some
    }
  }

  private def outside(ij:Cell) = {
    val i = ij.x
    val j = ij.y
    i < 0 || j < 0 || i > cols - 1 || j > rows - 1
  }

  def swap(ij:Cell, pq:Cell) = {
    val k = coordsToIndex(ij, cols)
    val r = coordsToIndex(pq, cols)
    if (outside(ij) || outside(pq)) {
      this
    } else {
      val temp = elements(k)
      new Array2d(elements.updated(k, elements(r)).updated(r, temp), cols, rows)
    }
  }

  def first:Option[T] = getOption((0,0))

  def countT(f:T => Boolean):Int = {
    foldLeft(0) { case (num, (_, t)) =>
      num + f(t).select(0, 1)
    }
  }

  def foldLeft[R](r:R)(f:(R, (Cell,T)) => R): R = {
    elements.zipWithIndex.foldLeft(r) { case (acc, (e, k)) =>
      val ij = indexToCoords(k, cols)
      f(acc,(ij, e))
    }
  }

  def get(ij:Cell):T = {
    val k = coordsToIndex(ij, cols)
    elements(k)
  }

  def getOption(ij:Cell):Option[T] = {
    if (outside(ij)) {
      None
    } else {
      get(ij).some
    }
  }
}
