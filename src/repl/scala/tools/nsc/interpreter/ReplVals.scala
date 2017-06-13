/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.language.implicitConversions
import scala.reflect.api.{Universe => ApiUniverse}
import scala.reflect.runtime.{universe => ru}

/** A class which the repl utilizes to expose predefined objects.
 *  The base implementation is empty; the standard repl implementation
 *  is StdReplVals.
 */
abstract class ReplVals { }

class StdReplVals(final val intp: IMain) extends ReplVals {
  // TODO bring back access to shell features from the interpreter?
  // The repl backend has now cut its ties to the shell, except for the ReplReporter interface
  // Before, we gave the user access to: repl, reader, isettings (poor name), completion and history.
  // We could bring back some of this functionality if desired by adding it to ReplReporter
  final val vals                          = this
  final lazy val power                    = intp.power
  final lazy val phased                   = power.phased
  final lazy val global: intp.global.type = intp.global
  final lazy val analyzer                 = global.analyzer

  object treedsl extends { val global: intp.global.type = intp.global } with ast.TreeDSL { }

  final lazy val typer = analyzer.newTyper(
    analyzer.rootContext(
      power.unit("").asInstanceOf[analyzer.global.CompilationUnit]
    )
  )
  def lastRequest = intp.lastRequest

  class ReplImplicits extends power.Implicits2

  final lazy val replImplicits = new ReplImplicits

  def typed[T <: analyzer.global.Tree](tree: T): T = typer.typed(tree).asInstanceOf[T]
}
