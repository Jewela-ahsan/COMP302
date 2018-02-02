import scala.io.Source

object q5 {
  def shoelace(filename: String) : Double = {
    def getPoints(filename: String) : Array[(Double, Double)] = {
      val stringPoints = Source.fromFile(filename).mkString.split(Array(' ', '\n'))
      val points = for (p <- stringPoints if(!p.isEmpty)) yield p.toDouble
      for (p <- 0 until points.size if(p % 2 == 0)) yield (points(p), points(p+1))
    }
    def doShoelace(points: Array[(Double, Double)], n: Int) : Double = {
      if(n == 0) points(0)._1 * points(1)._2 - points(1)._1 * points(0)._2
      else (points(n)._1 * points(n+1)._2 + doShoelace(points, n-1))
        - (points(n+1)._1 * points(n)._2 + doShoelace(points, n-1))
    }
    val points = getPoints(filename)
    for(p <- points) println(p)
    Math.abs(doShoelace(points, points.size - 2)
      + points(points.size - 1)._1 * points(0)._2
      - points(0)._1 * points(points.size - 1)._2) / 2.0
  }

  def main(args: Array[String]): Unit = {
    println(shoelace("test.txt"))
  }
}
