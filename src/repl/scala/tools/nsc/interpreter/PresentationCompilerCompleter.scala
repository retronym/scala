package scala.tools.nsc.interpreter

import scala.reflect.internal.util.Position
import scala.tools.nsc.interpreter.Completion.{ScalaCompleter, Candidates}

class PresentationCompilerCompleter(intp: IMain) extends ScalaCompleter {

  override def complete(buf: String, cursor: Int): Candidates =
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
              val completions = typeMembers(pos1).toList.flatten.filter(_.sym.name.startsWith(name.encoded))
              val nameStart: Int = (qual.pos.end until focus1.pos.end).find( p =>
                result.unit.source.identifier(result.unit.source.position(p)).isDefined
              ).getOrElse(qual.pos.end + 2)
              val position: Int = cursor + nameStart - pos1.start + 1
              Candidates(position, completions.map(_.sym.name.decoded))
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
