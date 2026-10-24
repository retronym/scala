/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal
package tpe

import scala.collection.mutable.HashSet

private[internal] trait TypeToStrings {
  self: SymbolTable =>

  /** The maximum number of recursions allowed in toString
    */
  final val maxToStringRecursions = 50

  def toStringRecursions: Int = scratch.toStringRecursions
  def toStringRecursions_=(value: Int): Unit = scratch.toStringRecursions = value

  def toStringSubjects: HashSet[Type] = scratch.toStringSubjects

  protected def typeToString(tpe: Type): String = {
    val s = scratch
    // if (toStringSubjects contains tpe) {
    //   // handles self-referential anonymous classes and who knows what else
    //   "..."
    // }
    // else
    if (s.toStringRecursions >= maxToStringRecursions) {
      devWarning("Exceeded recursion depth attempting to print " + util.shortClassOfInstance(tpe))
      if (settings.isDebug)
        (new Throwable).printStackTrace

      "..."
    }
    else
      try {
        s.toStringRecursions += 1
        // TODO: study performance impact of this cache
        // to quote Jason:
        //   I'm a little uneasy with the performance impact of the fail-safe. We end up calling Type#toString
        //   when we generate error messages, including, importantly, errors issued during silent mode that are never issued.
        // toStringSubjects += tpe
        tpe.safeToString
      } finally {
        // toStringSubjects -= tpe
        s.toStringRecursions -= 1
      }
  }
}
