package scala.collection
package parallel.immutable



import org.ahmadsoft.ropes._
import scala.collection.parallel.EnvironmentPassingCombiner
import scala.collection.parallel.Combiner
import scala.collection.parallel.ParSeqIterator
import scala.collection.generic.CanCombineFrom




class ParRope private[immutable] (rope: Rope)
extends ParSeq[Char]
   with parallel.ParSeqLike[Char, ParRope, immutable.WrappedString]
{
self =>
  
  def this() = this(new RopeBuilder().build(new StringBuilder))
  
  override def toParSeq = this
  
  def apply(i: Int): Char = rope.charAt(i)
  
  def length = rope.length()
  
  def seq = error("unsupported")
  
  override protected[this] def newCombiner = ParRope.newCombiner
  
  type SCPI = SignalContextPassingIterator[ParRopeIterator]
  
  def parallelIterator: ParSeqIterator[Char] = new ParRopeIterator with SCPI
  
  class ParRopeIterator(var i: Int = 0, val until: Int = rope.length(), r: Rope = rope)
  extends super.ParIterator {
  me: SignalContextPassingIterator[ParRopeIterator] =>
    
    def hasNext = i < until
    
    def next = {
      val c = r.charAt(i)
      i += 1
      c
    }
    
    def remaining = until - i
    
    def dup = new ParRopeIterator(i, until, r) with SCPI
    
    def split: Seq[ParIterator] = if (remaining > 1) {
      val div = remaining / 2
      Seq(r.subSequence(i, i + div), r.subSequence(i + div, until)) map {
        nr => new ParRopeIterator(0, nr.length(), nr) with SCPI
      }
    } else Seq(this)
    
    def psplit(sizes: Int*): Seq[ParIterator] = {
      var left = r.subSequence(i, until)
      val parts = mutable.ArrayBuffer[Rope]()
      for (sz <- sizes) {
        parts += left.subSequence(0, sz min left.length)
        left = left.subSequence(sz min left.length, left.length)
      }
      parts map { nr => new ParRopeIterator(0, nr.length(), nr) with SCPI }
    }
    
  }
  
}


private class ParRopeCombiner()
extends parallel.Combiner[Char, ParRope] {
self: EnvironmentPassingCombiner[Char, ParRope] =>
  
  val chain = mutable.ArrayBuffer(new StringBuilder)
  var last = chain(0)
  
  def +=(elem: Char) = {
    last += elem
    this
  }
  
  def result = {
    var rope = new RopeBuilder().build(new StringBuilder)
    for (sb <- chain) {
      rope = rope.append(sb)
    }
    
    new ParRope(rope)
  }
  
  def size = chain.foldLeft(0)(_ + _.length)
  
  def clear = {
    chain.clear
    chain += new StringBuilder
    last = chain(0)
  }
  
  def combine[N <: Char, NewTo >: ParRope](other: Combiner[N, NewTo]): Combiner[N, NewTo] = if (this ne other) {
    val that = other.asInstanceOf[ParRopeCombiner]
    
    chain ++= that.chain
    last = chain.last
    
    this
  } else this
  
}


object ParRope {
  implicit def canBuildFrom: CanCombineFrom[ParRope, Char, ParRope] =
    new CanCombineFrom[ParRope, Char, ParRope] {
      def apply(from: ParRope): Combiner[Char, ParRope] = newCombiner
      def apply(): Combiner[Char, ParRope] = newCombiner
    }
  
  def newBuilder: Combiner[Char, ParRope] = newCombiner
  def newCombiner: Combiner[Char, ParRope] = new ParRopeCombiner() with EnvironmentPassingCombiner[Char, ParRope]
}














