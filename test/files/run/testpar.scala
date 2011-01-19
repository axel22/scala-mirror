


import scala.parallel._





object Test {
  
  def main(args: Array[String]) {
    assert(fib(40) == 102334155)
  }
  
  def fib(n: Int): Int = if (n < 3) 1 else if (n < 35) fib(n - 1) + fib(n - 2) else {
    val (p, pp) = par(fib(n - 1), fib(n - 2))
    p() + pp()
  }
  
}
