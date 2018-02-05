object q3 {

  def fib(n: Int): Int = {
    if (n < 2) 1 else fib(n - 1) + fib(n - 2)
  }

  /**
    * Computes the fibonacci of n
    * @author Kevin Laframboise - 260687529
    * @param n
    * @return fib(n)
    */
  def fib2(n: Int): Int = {

    /**
      * @param count fib(count) = fib_1 + fib_2
      * @param fib_1 fib_1 = fib(count - 2)
      * @param fib_2 fib_2 = fib(count - 1)
      * @param n     n of fib(n) that is being computed
      * @return fib(n), with fib(0) = 1, fib(1) = 1
      */
    def doFib(count: Int, fib_1: Int, fib_2: Int, n: Int): Int = {
      if (count < n) doFib(count + 1, fib_2, fib_1 + fib_2, n)  //increase count, fib_1 is now fib_2, fib_2 is fib_1 + fib_2
      else fib_1 + fib_2  //end of series reached, return fib_1 + fib_2 as fib(n)
    }

    if (n == 0) 1
    else if (n == 1) 1
    else doFib(2, 1, 1, n)

  }

  def main(args: Array[String]): Unit = {
    println(fib(10))
    println(fib(10))
  }
}