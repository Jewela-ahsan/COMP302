object q4 {
  /**
    * Determines whether it is possible to return just enough cakes to have exactly 40.
    * @author Kevin Laframboise - 260687529
    * @param n original number of cakes
    * @return true or false
    */
  def cakes(n : Int) : Boolean = {
    if(n < 40) false  // not enough cakes!
    else if(n == 40) true // just enough cakes!
    else if(n % 2 == 0 && n/2 >= 40) cakes(n/2) // if halving the number of cakes leaves with more or exactly 40 cakes, take half and continue the recursion
    else if((n % 3 == 0 || n % 4 == 0) && ((n/10)%10) * (n%10) != 0) cakes(n - ((n/10)%10) * (n%10)) // do this only if the 1st digit * the 2nd digit != 0 to avoid infinite loop while continuing the recursion
    else if(n % 5 == 0 && n-40 >= 40) cakes(n-40) // if removing 40 cakes leaves us with more or exactly 40 cakes, take 40 and continue the recursion
    else false  // when all options have been exhausted, return false
  }

  def main(args: Array[String]): Unit = {
    for(i <- 38 to 100 if cakes(i)) println(i)
  }
}
