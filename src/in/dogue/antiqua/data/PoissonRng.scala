package in.dogue.antiqua.data

import com.deweyvm.gleany.data.Point2d
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.math.{sin,cos}
class RandomQueue[T](seed:Long) {
  private val qSeed = seed
  private val random = new Random(qSeed)
  private val buffer = new ArrayBuffer[T]()
  def +=(item:T) = {
    buffer += item
  }

  def pop() : T = {
    val i = random.nextInt(buffer.length)
    swap(i, buffer.length - 1)
    buffer.remove(buffer.length - 1)
  }

  def isEmpty = buffer.length == 0

  def length = buffer.length

  private def swap(i:Int, j:Int) {
    val temp = buffer(i)
    buffer(i) = buffer(j)
    buffer(j) = temp
  }
}



class PoissonRng(val width:Double, val height:Double, minDist:(Int, Int) => Double, maxDist:Double, seed:Long) {
  private val random = new Random(seed)
  private val numPoints = 100
  //private val minDist = 10.0
  private val cellSize = maxDist/scala.math.sqrt(2)
  private val samplePoints = new ArrayBuffer[Point2d]()
  private val processList = new RandomQueue[Point2d](seed)
  private val cols:Int = (width/cellSize).toInt
  private val rows:Int = (height/cellSize).toInt
  private val grid = Array.fill[Option[Point2d]](cols, rows)(None)

  private def create() = {
    val first = Point2d(random.nextDouble*width, random.nextDouble*height)
    processList += first
    samplePoints += first
    while(!processList.isEmpty) {
      val point = processList.pop()

      for (i <- 0 until numPoints) {

        val next = generateRandomPointAround(point)

        if (!inNeighborhood(next) && inBounds(next)) {
          processList += next
          samplePoints += next
          val (i,j) = imageToGrid(next)

          grid(i)(j) = Some(next)
        }
      }
    }

  }


  private def inBounds(point:Point2d) = {
    point.x > 0 && point.x < width && point.y > 0 && point.y < height
  }

  private def inNeighborhood(point:Point2d):Boolean = {
    def tooClose(pp:Point2d) = {
      (pp - point).magnitude < minDist(pp.x.toInt, pp.y.toInt)
    }
    val nearby = squareAroundPoint(point).map({case (i,j) => grid(i)(j)})
    nearby.exists(_ exists tooClose)
  }

  private def imageToGrid(point:Point2d):(Int, Int) = {
    val i = (point.x / cellSize).toInt
    val j = (point.y / cellSize).toInt
    ((i + cols) % cols, (j + rows) % rows)
  }

  private val span:Int = scala.math.ceil(maxDist/cellSize).toInt
  private def squareAroundPoint(point:Point2d): IndexedSeq[(Int, Int)] = {
    val (i,j) = imageToGrid(point)
    for (ii <- (i - span) to (i + span);
         jj <- (j - span) to (j + span))
    yield ((ii + cols) % cols, (jj + rows) % rows)
  }

  private def generateRandomPointAround(point:Point2d) = {
    val r1 = random.nextDouble()
    val r2 = random.nextDouble()
    val radius = maxDist*(r1 + 1)
    val angle = 2 * Math.PI * r2
    val x = point.x + radius * cos(angle)
    val y = point.y + radius * sin(angle)
    //val (lx, ly) = lock(x, y)
    Point2d(x, y)

  }

  private def lock(x:Double, y:Double) = {
    ((x + width) % width, (y + height) % height)
  }



  create()
  private val points:Vector[Point2d] = samplePoints.toVector
  def getPoints = points

}
