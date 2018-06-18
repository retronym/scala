/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2018, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection

private[collection] case class MutableTuple2[K, V](var _1: K, var _2: V) extends Product2[K, V] {
  override def toString() = "(" + _1 + "," + _2 + ")"
}
