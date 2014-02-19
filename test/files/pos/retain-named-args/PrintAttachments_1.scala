import language.experimental.macros
import scala.reflect.macros.Context

object PrintAttachments {
  def printAttachments(a: Any): Unit = macro impl
  def impl(c: Context)(a: c.Tree): c.Tree = {
    import c.universe._
    a.collect {
      case Apply(fun, args) => 
        def render(arg: Tree) = {
          import scala.tools.nsc.typechecker.Analyzer
          val attach = arg.attachments.all.collect {
            case at: Analyzer#OriginalNamedArg => at
          }.toList
          attach match {
            case found :: Nil =>
              s"  $arg {$found}"
            case Nil => "  " + arg.toString
          }
        }
        println(s"$fun(\n${args map render mkString("\n")}\n)")
    }
    q"()"
  }
}
