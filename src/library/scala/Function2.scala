/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// generated by genprod on Sat Oct 16 11:19:09 PDT 2010 (with fancy comment) (with extra methods)

package scala




/** Function with 2 parameters.
 *  
 * In the following example the definition of
 *    max is a shorthand for the anonymous class
 *    definition anonfun2:
 *
 *  {{{
 *  object Main extends Application { 
 *    val max = (x: Int, y: Int) => if (x < y) y else x
 *
 *    val anonfun2 = new Function2[Int, Int, Int] {
 *      def apply(x: Int, y: Int): Int = if (x < y) y else x
 *    }
 *
 *    println(max(0, 1))
 *    println(anonfun2(0, 1))
 *  }
 *  }}}
 */
trait Function2[@specialized(scala.Int, scala.Long, scala.Double) -T1, @specialized(scala.Int, scala.Long, scala.Double) -T2, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +R] extends AnyRef { self =>
  def apply(v1:T1,v2:T2): R
  override def toString() = "<function2>"
  
  /** f(x1, x2)  == (f.curried)(x1)(x2)
   */
  def curried: T1 => T2 => R = {
    (x1: T1) => (x2: T2) => apply(x1, x2)
  }
  @deprecated("Use 'curried' instead")
  def curry = curried

  /* f(x1, x2) == (f.tupled)(Tuple2(x1, x2))
   */
  def tupled: Tuple2[T1, T2] => R = {
    case Tuple2(x1, x2) => apply(x1, x2)
  }

}
