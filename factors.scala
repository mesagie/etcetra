object primeFactors{
  def factorize(x: Int): List[Int] = {
  def foo(x: Int, a: Int): List[Int] = {
    (a*a < x, x % a) match {
        case (true, 0) => a :: foo(x/a, a)
        case (true, _) => foo(x, a+1)
        case (false, _) => List(x)
      }
    }
    foo(x, 2)
  }
}
