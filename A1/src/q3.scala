object q3 {
  def fib(n: Int): Int = {
    if (n < 2) 1 else fib(n - 1) + fib(n - 2)
  }

  def fib2(n: Int): Int = {

    /**
      *
      * @param count fib(count) = fib_1 + fib_2
      * @param fib_1 fib_1 = fib(count - 2)
      * @param fib_2 fib_2 = fib(count - 1)
      * @param n     n of fib(n) that is being computed
      * @return fib(n), with fib(0) = 0, fib(1) = 1
      */
    def doFib(count: Int, fib_1: Int, fib_2: Int, n: Int): Int = {
      if (count < n) doFib(count + 1, fib_2, fib_1 + fib_2, n)
      else fib_1 + fib_2
    }

    if (n == 0) 0
    else if (n == 1) 1
    else doFib(2, 0, 1, n)

  }
}