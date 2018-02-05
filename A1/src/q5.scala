import scala.io.Source

object q5 {
  /**
    * Computes the area of a polygon given the coordinates of its points.
    * @author Kevin Laframboise - 260687529
    * @param filename filename of the file containing the coordinates
    * @return the area of the polygon defines by the coordinates
    */
  def shoelace(filename: String) : Double = {

    /**
      * Gets the points from the given file.
      * @param filename
      * @return an IndexedSeq of Double Tuples representing the points
      */
    def getPoints(filename: String) : IndexedSeq[(Double, Double)] = {
      val stringPoints = Source.fromFile(filename).mkString.split(Array(' ', '\n')) // get the points in string form, stripping the whitespaces
      val points = for (p <- stringPoints if(!p.isEmpty)) yield p.toDouble // convert the points to doubles
      for (p <- 0 until points.size if(p % 2 == 0)) yield (points(p), points(p+1)) // form tuples
    }

    /**
      * Computes the are using the parsed points
      * @param points
      * @param n
      * @return the area of the polygon
      */
    def doShoelace(points: IndexedSeq[(Double, Double)], n: Int) : Double = {
      if(n == 0) (points(0)._1 * points(1)._2) - (points(1)._1 * points(0)._2)  //base case, see formula for i = 1
      else (points(n)._1 * points(n+1)._2) - (points(n+1)._1 * points(n)._2) + doShoelace(points, n-1) //compute the nth term of the series and add it to the rest
    }

    val points = getPoints(filename)  // get the points as a list of tuples

    Math.abs(doShoelace(points, points.size - 2)
      + points(points.size - 1)._1 * points(0)._2
      - points(0)._1 * points(points.size - 1)._2) / 2.0  // compute the area using the shoelace formula
  }

  def main(args: Array[String]): Unit = {
    println(shoelace("test.txt"))
  }
}
