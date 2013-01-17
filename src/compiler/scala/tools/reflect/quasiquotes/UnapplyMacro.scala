package scala.tools.reflect

import java.util.UUID.randomUUID
import scala.tools.nsc.Global
import scala.reflect.macros.runtime.Context
import scala.collection.mutable
import scala.collection.SortedMap

trait UnapplyMacro { self: Quasiquotes =>
  import c.universe._
  import c.universe.Flag._

  def unapplyQ: Tree = unapply(new QParser)

  def unapplyTq: Tree = unapply(new TQParser)

  def unapply(parser: QParser): Tree = {

    val (universe, parts) =
      c.macroApplication match {
        case q"$universe.QuasiQuote($stringContext.apply(..$parts0)).${_}.unapply(${_})" =>
          val parts = parts0.map{
            case Literal(Constant(s: String)) => s
            case _ => throw new Exception("") // empty exception?
          }
          (universe, parts)
        case _ => throw new Exception("") // empty exception?
      }

    if (!(parts.length >= 1 && parts.length <= 23))
      c.abort(c.enclosingPosition, "Inappropriate amount of quasiquote params.")

    val unapplySelector = Ident(nme.SELECTOR_DUMMY)
    unapplySelector.setType(memberType(universe.tpe, tpnme.Tree))

    val (code, placeholders) = {
      val sb = new StringBuilder()
      var placeholders = SortedMap[String, (Tree, String)]()

      parts.init.foreach { p =>
        val (part, cardinality) =
          if (p.endsWith("..."))
            (p.stripSuffix("..."), "...")
          else if (p.endsWith(".."))
            (p.stripSuffix(".."), "..")
          else
            (p, "")
        val freshname = c.fresh(nme.QUASIQUOTE_PREFIX)
        sb.append(part)
        sb.append(freshname)
        placeholders += freshname -> (EmptyTree, cardinality)
      }
      sb.append(parts.last)

      (sb.toString, placeholders)
    }

    if (settings.Yquasiquotedebug.value) println(s"code to parse=\n$code\n")

    val tree = parser.parse(code, placeholders.keys.toSet)

    if (settings.Yquasiquotedebug.value) println(s"parsed tree\n=${tree}\n=${showRaw(tree)}\n")

    val reifier = new UnapplyReifier(universe, placeholders)
    val reifiedTree = reifier.reifyTree(tree)
    val correspondingTypes = reifier.correspondingTypes

    if (settings.Yquasiquotedebug.value) println(s"corresponding types=\n$correspondingTypes\n")
    if (settings.Yquasiquotedebug.value) println(s"reified tree\n=${reifiedTree}\n=${showRaw(reifiedTree)}\n")

    val caseBody: Tree =
      if (placeholders.size == 0)
        q"true"
      else if (placeholders.size == 1)
        q"Some(${TermName(placeholders.keys.head)})"
      else {
        val tupleN = TermName("Tuple" + placeholders.size.toString)
        val tupleArgs = placeholders.map(p => Ident(TermName(p._1)))
        q"Some($tupleN(..$tupleArgs))"
      }

    val unapplyResultType: Tree =
      if (placeholders.size == 0) {
        tq"Boolean"
      } else if (placeholders.size == 1) {
        val tpe = correspondingTypes(placeholders.keys.head)
        tq"Option[$tpe]"
      } else {
        val tupleN = TypeName("Tuple" + placeholders.size)
        val targs = placeholders.map(p => correspondingTypes(p._1))
        tq"Option[$tupleN[..$targs]]"
      }

    val moduleName = TermName(nme.QUASIQUOTE_MATCHER_NAME + randomUUID().toString.replace("-", ""))

    val moduleDef = {
      val u = nme.UNIVERSE_SHORT
      q"""
      object $moduleName {
        def unapply($u: scala.reflect.api.Universe)(tree: $u.Tree): $unapplyResultType = {
          // importing type tags from universe for tree pattern matching to work as expected
          import $u._
          tree match {
            case $reifiedTree => $caseBody
            case _ => None
          }
        }
      }
      """
    }

    if (settings.Yquasiquotedebug.value) println(s"moduledef\n=${showRaw(moduleDef, printTypes=true, printIds=true)}\n=$moduleDef\n")

    val packge = nme.QUASIQUOTE_MATCHER_PACKAGE
    val modulePos = c.enclosingPosition.focus

    c.introduceTopLevel(packge, atPos(modulePos)(moduleDef))

    q"$packge.$moduleName.unapply($universe)($unapplySelector)"
  }
}
