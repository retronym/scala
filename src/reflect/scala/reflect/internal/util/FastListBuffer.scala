/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.reflect.internal.util

import scala.collection.immutable.{::, Nil, List}

// The claims of "Fast" stem from the absense of inheritance from the collections heirarchy
// which have a number of non-trivial constructors
final class FastListBuffer[A] {

  /** Expected invariants:
   *  If start.isEmpty, last0 == null
   *  If start.nonEmpty, last0 != null
   *  If len == 0, start.isEmpty
   *  If len > 0, start.nonEmpty
   */
  private var start: List[A] = Nil
  private var last0: ::[A] = _
  private var len = 0

  /** Appends a single element to this buffer. This operation takes constant time.
   *
   *  @param x  the element to append.
   *  @return   this $coll.
   */
  def += (x: A): this.type = {
    if (len <= 0) {
      last0 = new :: (x, Nil)
      start = last0
    } else {
      val last1 = last0
      last0 = new :: (x, Nil)
      last1.tl = last0
    }
    len += 1
    this
  }

  /** Clears the buffer contents.
   */
  def clear() {
    start = Nil
    last0 = null
    len = 0
  }

  /** Prepends a single element to this buffer. This operation takes constant
   *  time.
   *
   *  @param x  the element to prepend.
   *  @return   this $coll.
   */
  def +=: (x: A): this.type = {
    val newElem = new :: (x, start)
    if (len <= 0) last0 = newElem
    start = newElem
    len += 1
    this
  }

  def toList: List[A] = {
    start
  }
}
