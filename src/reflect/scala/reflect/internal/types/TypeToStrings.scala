package scala.reflect
package internal
package types

import scala.collection.{ mutable, immutable, generic }
import generic.Clearable
import scala.ref.WeakReference
import mutable.ListBuffer
import Flags._
import scala.util.control.ControlThrowable
import scala.annotation.tailrec
import util.Statistics
import scala.runtime.ObjectRef
import util.ThreeValues._
import Variance._

trait TypeToStrings extends api.Types {
  self: SymbolTable =>

  /** The maximum number of recursions allowed in toString
    */
  final val maxTostringRecursions = 50

  /*TODO private*/ var tostringRecursions = 0

  protected def typeToString(tpe: Type): String =
    if (tostringRecursions >= maxTostringRecursions) {
      devWarning("Exceeded recursion depth attempting to print " + util.shortClassOfInstance(tpe))
      if (settings.debug.value)
        (new Throwable).printStackTrace

      "..."
    }
    else
      try {
        tostringRecursions += 1
        tpe.safeToString
      } finally {
        tostringRecursions -= 1
      }
}
