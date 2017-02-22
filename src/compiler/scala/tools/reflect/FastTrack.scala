package scala.tools
package reflect

import scala.reflect.reify.Taggers
import scala.tools.nsc.typechecker.{ Analyzer, Macros }
import scala.reflect.runtime.Macros.currentMirror
import scala.reflect.quasiquotes.{ Quasiquotes => QuasiquoteImpls }

/** Optimizes system macro expansions by hardwiring them directly to their implementations
 *  bypassing standard reflective load and invoke to avoid the overhead of Java/Scala reflection.
 */
class FastTrack[MacrosAndAnalyzer <: Macros with Analyzer](val macros: MacrosAndAnalyzer) {

  import macros._
  import global._
  import definitions._
  import scala.language.implicitConversions
  import treeInfo.Applied

  def contains(symbol: Symbol): Boolean = fastTrackFor(symbol) != null
  def apply(symbol: Symbol): FastTrackEntry = { val result = fastTrackFor(symbol); if (result == null) throw new NoSuchElementException(symbol.defString) else result }
  def get(symbol: Symbol): Option[FastTrackEntry] = Option(fastTrackFor(symbol))

  private implicit def context2taggers(c0: MacroContext): Taggers { val c: c0.type } =
    new { val c: c0.type = c0 } with Taggers
  private implicit def context2macroimplementations(c0: MacroContext): FormatInterpolator { val c: c0.type } =
    new { val c: c0.type = c0 } with FormatInterpolator
  private implicit def context2quasiquote(c0: MacroContext): QuasiquoteImpls { val c: c0.type } =
    new { val c: c0.type = c0 } with QuasiquoteImpls
  private def makeBlackbox(owner: Symbol, name: Name)(pf: PartialFunction[Applied, MacroContext => Tree]) =
    (owner, name, new FastTrackEntry(pf, isBlackbox = true))
  private def makeWhitebox(owner: Symbol, name: Name)(pf: PartialFunction[Applied, MacroContext => Tree]) =
    (owner, name, new FastTrackEntry(pf, isBlackbox = false))

  final class FastTrackEntry(pf: PartialFunction[Applied, MacroContext => Tree], val isBlackbox: Boolean) extends (MacroArgs => Any) {
    def validate(tree: Tree) = pf isDefinedAt Applied(tree)
    def apply(margs: MacroArgs): margs.c.Expr[Nothing] = {
      val MacroArgs(c, _) = margs
      // Macros validated that the pf is defined here - and there's not much we could do if it weren't.
      c.Expr[Nothing](pf(Applied(c.expandee))(c))(c.WeakTypeTag.Nothing)
    }
  }

  private[this] val materializeClassTagEntry = new FastTrackEntry({    case Applied(_, ttag :: Nil, _)                 => _.materializeClassTag(ttag.tpe) }, isBlackbox = true)
  private[this] val materializeWeakTypeTagEntry = new FastTrackEntry({ case Applied(_, ttag :: Nil, (u :: _) :: _)     => _.materializeTypeTag(u, EmptyTree, ttag.tpe, concrete = false) }, isBlackbox = true)
  private[this] val materializeTypeTagEntry = new FastTrackEntry({     case Applied(_, ttag :: Nil, (u :: _) :: _)     => _.materializeTypeTag(u, EmptyTree, ttag.tpe, concrete = true) }, isBlackbox = true)
  private[this] val reifyEntry = new FastTrackEntry({                  case Applied(_, ttag :: Nil, (expr :: _) :: _)  => c => c.materializeExpr(c.prefix.tree, EmptyTree, expr) }, isBlackbox = true)
  private[this] val stringContextFEntry = new FastTrackEntry({         case _                                          => _.interpolate }, isBlackbox = true)
  private[this] val currentMirrorEntry = new FastTrackEntry({          case _                                          => c => currentMirror(c).tree }, isBlackbox = true)
  private[this] val quasiquoteApplyEntry = new FastTrackEntry({        case _                                          => _.expandQuasiquote }, isBlackbox = false)
  private[this] val quasiquoteUnapplyEntry = new FastTrackEntry({      case _                                          => _.expandQuasiquote }, isBlackbox = false)

  private def fastTrackFor(sym: Symbol): FastTrackEntry = {
    if (!sym.hasTransOwner(ScalaPackageClass)) null
    else {
      val topLevelOwner = sym.enclosingTopLevelClass
      val name = sym.name
      if (topLevelOwner == ReflectPackage.moduleClass && name == nme.materializeClassTag) materializeClassTagEntry
      else if (topLevelOwner == ReflectApiPackage.moduleClass && name == nme.materializeWeakTypeTag) materializeWeakTypeTagEntry
      else if (topLevelOwner == ReflectApiPackage.moduleClass && name == nme.materializeTypeTag) materializeTypeTagEntry
      else if (topLevelOwner == ApiUniverseClass && name == nme.reify) reifyEntry
      else if (topLevelOwner == StringContextClass && name == nme.f) stringContextFEntry
      else if (topLevelOwner == ReflectRuntimePackage.moduleClass && name == nme.currentMirror) currentMirrorEntry
      else if (topLevelOwner == ApiUniverseClass && sym.owner.name == tpnme.Quasiquote) {
        if (name == nme.apply) quasiquoteApplyEntry
        else if (name == nme.unapply) quasiquoteApplyEntry
        else null
      }
      else null
    }
  }
}
