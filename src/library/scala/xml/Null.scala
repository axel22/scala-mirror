/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.xml

import Utility.{ isNameStart }

/** Essentially, every method in here is a dummy, returning Zero[T].
 *  It provides a backstop for the unusual collection defined by MetaData,
 *  sort of a linked list of tails.
 */
case object Null extends MetaData {    
  override def iterator = Iterator.empty
  override def append(m: MetaData, scope: NamespaceBinding = TopScope): MetaData = m
  override def filter(f: MetaData => Boolean): MetaData = this

  def copy(next: MetaData) = next
  def getNamespace(owner: Node) = null

  override def hasNext = false
  def next = null
  def key = null
  def value = null
  def isPrefixed = false

  override def length = 0
  override def length(i: Int) = i
  
  override def strict_==(other: Equality) = other match {
    case x: MetaData  => x.length == 0
    case _            => false
  }
  override def basisForHashCode: Seq[Any] = Nil

  def apply(namespace: String, scope: NamespaceBinding, key: String) = null
  def apply(key: String) = {
    if (!isNameStart(key.head))
      throw new IllegalArgumentException("not a valid attribute name '"+key+"', so can never match !")
      
    null
  }

  def toString1(sb: StringBuilder) = ()
  override def toString1(): String = ""
  override def toString(): String = ""

  override def buildString(sb: StringBuilder): StringBuilder = sb
  override def wellformed(scope: NamespaceBinding) = true

  def remove(key: String) = this
  def remove(namespace: String, scope: NamespaceBinding, key: String) = this
}
