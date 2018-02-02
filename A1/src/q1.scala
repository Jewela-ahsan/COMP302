object q1 {
  def euler(count: Int, result: Double = 0): Double = {
    if (count == 0) Math.sqrt(6 * result)
    else euler(count - 1, result + 1 / Math.pow(count, 2))
  }
}

