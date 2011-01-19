



import scala.parallel._




object Test {
  
  def main(args: Array[String]) {
    blockcomp(10)
  }
  
  val lock = new java.util.concurrent.locks.ReentrantLock
  
  def blockcomp(n: Int): Unit = if (n > 0) {
    val (x, y) = par(blockcomp(n - 1), blockcomp(n - 1))
    if (n == 8) blocking { // without this blocking block, deadlock occurs
      lock.lock()
    }
    x()
    y()
    if (n == 8) {
      lock.unlock()
    }
  }
  
}
