package in.dogue.antiqua.data

import in.dogue.antiqua.Implicits._

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

  def indexToCoords(k:Int, cols:Int):(Int,Int) = (k % cols, k / cols)
  def coordsToIndex(i:Int, j:Int, cols:Int):Int = i + j*cols
}



class Array2d[T](private val elements:Vector[T],
                 val cols :Int,
                 val rows :Int) {
  outer =>
  import Array2d._

  def strictGetAll:IndexedSeq[T] = elements

  def count(f:(Int,Int,T)=>Boolean):Int = {
    elements.zipWithIndex.count {case (t, k) =>
      val (i, j) = indexToCoords(k, cols)
      f(i, j, t)
    }
  }

  def map[K](f:(Int,Int,T)=>K) = {
    Array2d.tabulate(cols, rows) { case (i, j) =>
      val elt = get(i, j)
      f(i, j, elt)
    }
  }

  def flatten:Seq[(Int,Int,T)] = {
    map[(Int,Int,T)]{case (x, y, z) => (x, y, z)}.elements

  }

  def update(i:Int, j:Int, f:T => T) = {
    val k = coordsToIndex(i, j, cols)
    getOption(i, j).map {t =>
      new Array2d(elements.updated(k, f(t)), cols, rows)
    }.getOrElse{
      this
    }
  }

  def updated(i:Int, j:Int, t:T) = {
    val k = coordsToIndex(i, j, cols)
    new Array2d(elements.updated(k, t), cols, rows)
  }

  def groupBy[K](f:T => K): Map[K, Vector[T]] = elements.groupBy(f)

  def max[K >: T](implicit cmp: Ordering[K]):T = elements.max(cmp)
  def min[K >: T](implicit cmp: Ordering[K]):T = elements.min(cmp)

  def find(f:T=> Boolean):Option[(Int, Int, T)] = {
    elements.zipWithIndex.find {case (e, i) =>
      f(e)
    }.flatMap{case (e, i) =>
      val (x, y) = indexToCoords(i, cols)
      (x, y, e).some
    }
  }

  private def outside(i:Int, j:Int) = {
    i < 0 || j < 0 || i > cols - 1 || j > rows - 1
  }

  def swap(i:Int, j:Int, p:Int, q:Int) = {
    val k = coordsToIndex(i, j, cols)
    val r = coordsToIndex(p, q, cols)
    if (outside(i, j) || outside(p, q)) {
      this
    } else {
      val temp = elements(k)
      new Array2d(elements.updated(k, elements(r)).updated(r, temp), cols, rows)
    }
  }

  def foldLeft[R](r:R)(f:(R, (Int,Int,T)) => R): R = {
    elements.zipWithIndex.foldLeft(r) { case (acc, (e, k)) =>
      val (i, j) = indexToCoords(k, cols)
      f(acc,(i, j, e))
    }
  }

  def get(i:Int, j:Int):T = {
    val k = coordsToIndex(i, j, cols)
    elements(k)
  }

  def getOption(i:Int, j:Int):Option[T] = {
    if (i < 0 || j < 0 || i > cols - 1 || j > rows - 1) {
      None
    } else {
      get(i, j).some
    }
  }
}
