object q4 {
  def cakes(n : Int) : Boolean = {
    if(n < 40) false
    else if(n == 40) true
    else if(n % 2 == 0 && n/2 >= 40) cakes(n/2)
    else if((n % 3 == 0 || n % 4 == 0) && ((n/10)%10) * (n%10) != 0) cakes(n - ((n/10)%10) * (n%10))
    else if(n % 5 == 0) cakes(n-40)
    else false
  }
}
