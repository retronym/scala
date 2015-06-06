package scala.tools.nsc.interpreter

import scala.reflect.internal.util.Position
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.interpreter.Completion.{ScalaCompleter, Candidates}

class PresentationCompilerCompleter(intp: IMain) extends ScalaCompleter {
  private case class Request(line: String, cursor: Int)
  private var lastRequest = new Request("", -1)
  private var tabCount = 0

  override def complete(buf: String, cursor: Int): Candidates = {
    val request = new Request(buf, cursor)
    if (request == lastRequest) tabCount += 1 else { tabCount = 0; lastRequest = request}
    intp.presentationCompile(buf, false) match {
      case Left(_) =>
        Completion.NoCandidates
      case Right(result) =>
        try {
          val offset = result.preambleLength
          val pos1 = result.unit.source.position(offset + cursor)
          import result.compiler._
          val focus1: Tree = typedTreeAt(pos1)
          focus1 match {
            case Select(qual, name) =>
              val prefix = if (name == nme.ERROR) "" else name.encoded
              val allTypeMembers = typeMembers(qual.pos).toList.flatten
              val completions = allTypeMembers.filter(_.sym.name.startsWith(prefix))
              def fallback = qual.pos.end + 2
              val nameStart: Int = (qual.pos.end + 1 until focus1.pos.end).find(p =>
                result.unit.source.identifier(result.unit.source.position(p)).exists(_.length > 0)
              ).getOrElse(fallback)
              val position: Int = cursor + nameStart - pos1.start
              if (tabCount > 0 && completions.forall(_.sym.name == name)) {
                val defStrings = completions.flatMap(_.sym.alternatives).map(sym => sym.defStringSeenAs(qual.tpe memberType sym))
                Candidates(cursor, "" :: defStrings)
              } else {
                val memberCompletions: List[String] = completions.map(_.sym.name.decoded)
                Candidates(position, memberCompletions)
              }
            case Ident(name) =>
              val allMembers = scopeMembers(pos1)
              val completions = allMembers.filter(_.sym.name.startsWith(name.encoded))
              val position: Int = cursor + focus1.pos.start - pos1.start
              Candidates(position, completions.map(_.sym.name.decoded))
            case _ =>
              Completion.NoCandidates
          }
        } finally result.cleanup()
    }
  }
}
