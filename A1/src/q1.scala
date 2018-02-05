object q1 {

  /**
    * Extracts PI from the infinite series 1/(n*n)
    * @author Kevin Laframboise - 260687529
    * @param count dictates how many terms of the series are computed
    * @param result optional, used in the recursion to store the intermediate result (default = 0)
    * @return PI
    */
  def euler(count: Int, result: Double = 0): Double = {
    if (count == 0) Math.sqrt(6 * result)
    else euler(count - 1, result + 1 / Math.pow(count, 2))
  }
}

