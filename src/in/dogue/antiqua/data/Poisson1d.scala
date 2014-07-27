package in.dogue.antiqua.data

import scala.util.Random

class Poisson1d(start:Double, minDist:Double, maxDist:Double, end:Double, seed:Long) {
  val r = new Random(seed)

  def get = generate(start, Seq())

  private def generate(last:Double, sofar:Seq[Double]):Seq[Double] = {
    val next = last + minDist + r.nextDouble()*(maxDist - minDist)
    if (next > end) {
      sofar
    } else {
      generate(next, next +: sofar)
    }
  }

}
