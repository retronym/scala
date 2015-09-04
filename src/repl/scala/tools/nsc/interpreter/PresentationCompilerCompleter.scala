/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc.interpreter

import scala.reflect.internal.Flags
import scala.reflect.internal.util.StringOps
import scala.tools.nsc.interpreter.Completion.{ScalaCompleter, Candidates}
import scala.tools.nsc.reporters.{AbstractReporter, StoreReporter}
import scala.util.control.NonFatal

class PresentationCompilerCompleter(intp: IMain) extends Completion with ScalaCompleter {
  import PresentationCompilerCompleter._
  import intp.{PresentationCompileResult => Result}

  private type Handler = Result => Candidates

  private var lastRequest = NoRequest
  private var tabCount = 0
  private var lastCommonPrefixCompletion: Option[String] = None
  private val CamelRegex = "([A-Z][^A-Z]*)".r
  private def camelComponents(s: String): List[String] = {
    CamelRegex.findAllIn("X" + s).toList match { case head :: tail => head.drop(1) :: tail; case Nil => Nil }
  }

  def resetVerbosity(): Unit = { tabCount = 0 ; lastRequest = NoRequest }
  def completer(): ScalaCompleter = this

  // A convenience for testing
  def complete(before: String, after: String = ""): Candidates = complete(before + after, before.length)
  override def complete(buf: String, cursor: Int): Candidates = {
    val request = Request(buf, cursor)
    if (request == lastRequest)
      tabCount += 1
    else {
      tabCount = 0
      lastRequest = request
    }

    // secret handshakes
    val slashPrint  = """.*// *print *""".r
    val slashTypeAt = """.*// *typeAt *(\d+) *(\d+) *""".r
    val Cursor = IMain.DummyCursorFragment

    def print(result: Result) = {
      val offset = result.preambleLength
      val pos1 = result.unit.source.position(offset).withEnd(offset + buf.length)
      import result.compiler._
      val tree = new Locator(pos1) locateIn result.unit.body match {
        case Template(_, _, constructor :: (rest :+ last)) => if (rest.isEmpty) last else Block(rest, last)
        case t => t
      }
      println("")
      result.compiler.reporter match {
        case sr: StoreReporter =>
          sr.infos.foreach(i => {
            val ar = intp.reporter.asInstanceOf[AbstractReporter]
            val severity = ar.severityFor(i.severity.id)
            ar.display(i.pos, i.msg, severity)
          })
        case _ => ""
      }
      val printed = showCode(tree) + " // : " + tree.tpe.safeToString + "\n"
      Candidates(cursor, "" :: printed :: Nil)
    }
    def typeAt(result: Result, start: Int, end: Int) = {
      val tpString = result.compiler.exitingTyper(result.typedTreeAt(buf, start, end).tpe.toString)
      Candidates(cursor, "" :: tpString :: Nil)
    }
    def candidates(result: Result): Candidates = {
      import result.compiler._
      import CompletionResult._
      def defStringCandidates(matching: List[Member], name: Name): Candidates = {
        val defStrings = for {
          member <- matching
          if member.symNameDropLocal == name
          sym <- member.sym.alternatives
          sugared = sym.sugaredSymbolOrSelf
        } yield {
            val tp = member.prefix memberType sym
            sugared.defStringSeenAs(tp)
          }
        Candidates(cursor, "" :: defStrings.distinct)
      }
      val found = result.completionsAt(cursor) match {
        case NoResults => Completion.NoCandidates
        case r =>
          def isInterpreterWrapperMember(m: Member): Boolean =
            definitions.isUniversalMember(m.sym) && nme.isReplWrapperName(m.prefix.typeSymbol.name)
          val matching = r.matchingResults().filterNot(isInterpreterWrapperMember)
          val tabAfterCommonPrefixCompletion = lastCommonPrefixCompletion.contains(buf.substring(0, cursor)) && matching.exists(_.symNameDropLocal == r.name)
          val doubleTab = tabCount > 0 && matching.forall(_.symNameDropLocal == r.name)
          if (tabAfterCommonPrefixCompletion || doubleTab) defStringCandidates(matching, r.name)
          else {
            if (matching.isEmpty && r.name.endsWith("__") && r.name.isTermName) {
              r.tree.attachments.get[ExpectedTypeAttachment[_]] match {
                case Some(ExpectedTypeAttachment(pt)) =>
                  val strippedName = r.name.stripSuffix("__")
                  val anyNameResults: List[Member] = r.results.filter(m => CompletionResult.prefixMatcher(m, strippedName))
                  val expectedTypeMatching1 = anyNameResults.filter(x => {
                    x.sym.name.isTermName && (x.prefix memberType x.sym).finalResultType <:< pt.asInstanceOf[Type]
                  })
                  val expectedTypeMatching2 = anyNameResults.filter(x => {
                    x.sym.name.isTermName && deriveTypeWithWildcards(x.sym.typeParams)((x.prefix memberType x.sym).finalResultType) <:< pt.asInstanceOf[Type]
                  })
                  val memberCompletions: List[String] = (expectedTypeMatching1 ++ expectedTypeMatching2).map(_.symNameDropLocal.decoded).distinct.sorted
                  println(s"\n${r.name} : $pt")
                  Candidates(cursor, "" :: memberCompletions)
                case _ => Completion.NoCandidates
              }
            } else if (matching.isEmpty){
              val anyNameResults: List[Member] = r.results.filter(m => CompletionResult.prefixMatcher(m, nme.EMPTY))
              val camelComponents1 = camelComponents(r.name.toString)
              val camelMatches = anyNameResults.filter(x => {
                camelComponents1.forall(component => x.sym.name.containsName(component)) && {
                  val camelComponents2 = camelComponents(x.sym.name.toString)
                  (camelComponents1 corresponds camelComponents2.take(camelComponents1.length))((x, y) => y.startsWith(x))
                }
              })
              val memberCompletions: List[String] = camelMatches.map(_.symNameDropLocal.decoded).distinct.sorted
              Candidates(cursor + r.positionDelta, memberCompletions)
            } else if (matching.nonEmpty && matching.forall(_.symNameDropLocal == r.name))
              Completion.NoCandidates // don't offer completion if the only option has been fully typed already
            else {
              // regular completion
              val memberCompletions: List[String] = matching.map(_.symNameDropLocal.decoded).distinct.sorted
              Candidates(cursor + r.positionDelta, memberCompletions)
            }
          }
      }
      lastCommonPrefixCompletion =
        if (found != Completion.NoCandidates && buf.length >= found.cursor)
          Some(buf.substring(0, found.cursor) + StringOps.longestCommonPrefix(found.candidates))
        else
          None
      found
    }
    val buf1 = buf.patch(cursor, Cursor, 0)
    try {
      intp.presentationCompile(buf1) match {
        case Left(_) => Completion.NoCandidates
        case Right(result) => try {
          buf match {
            case slashPrint() if cursor == buf.length => print(result)
            case slashTypeAt(start, end) if cursor == buf.length => typeAt(result, start.toInt, end.toInt)
            case _ => candidates(result)
          }
        } finally result.cleanup()
      }
    } catch {
      case NonFatal(e) =>
        if (isReplDebug) e.printStackTrace()
        Completion.NoCandidates
    }
  }
}
object PresentationCompilerCompleter {
  private case class Request(line: String, cursor: Int)
  private val NoRequest = Request("", -1)
}
