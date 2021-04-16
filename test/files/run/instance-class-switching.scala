import scala.tools.partest._

object Test extends DirectTest {
  override def extraSettings = "-usejavacp"
  override def show()        = {
    Console.withErr(Console.out) {
      compileString(newCompiler())(predefined)
      compile("-cp", s"$testOutput", "-stop:cleanup", "-Vprint:patmat,cleanup")
    }
  }

  def predefined = """
      |sealed trait Formatted
      |final case class Infix(infix: Formatted, left: Formatted, right: Formatted, top: Boolean) extends Formatted
      |final case class Simple(tpe: String)                                                      extends Formatted
      |final case class Qualified(path: List[String], tpe: String)                               extends Formatted
      |      case object UnitForm                                                                extends Formatted
      |final case class Applied(cons: Formatted, args: List[Formatted])                          extends Formatted
      |final case class TupleForm(elems: List[Formatted])                                        extends Formatted
      |final case class FunctionForm(args: List[Formatted], ret: Formatted, top: Boolean)        extends Formatted
      |final case class RefinedForm(elems: List[Formatted], decls: List[Formatted])              extends Formatted
      |final case class Diff(left: Formatted, right: Formatted)                                  extends Formatted
      |final case class Decl(sym: Formatted, rhs: Formatted)                                     extends Formatted
      |final case class DeclDiff(sym: Formatted, left: Formatted, right: Formatted)              extends Formatted
      |final case class ByName(tpe: Formatted)                                                   extends Formatted
    """.stripMargin.trim

  def code = """
       |object ShowFormatted {
       |  def show(f: Formatted): String = f match {
       |    case Infix(left, _, _, _)       => s"${show(left)}"
       |    case Simple(tpe)                => s"$tpe"
       |    case Qualified(Nil, tpe)        => s"$tpe"
       |    case Qualified(path, tpe)       => s"${path.mkString}$tpe"
       |    case UnitForm                   => s"()"
       |    case Applied(cons, _)           => s"${show(cons)}"
       |    case TupleForm(Nil)             => s"()"
       |    case TupleForm(h :: _)          => s"${show(h)}"
       |    case FunctionForm(Nil, ret, _)  => s"${show(ret)}"
       |    case FunctionForm(h :: _, _, _) => s"${show(h)}"
       |    case RefinedForm(Nil, _)        => s"()"
       |    case RefinedForm(h :: _, _)     => s"${show(h)}"
       |    case Diff(l, _)                 => s"${show(l)}"
       |    case Decl(sym, _)               => s"${show(sym)}"
       |    case DeclDiff(sym, _, _)        => s"${show(sym)}"
       |    case ByName(tpe)                => s"${show(tpe)}"
       |  }
       |}
    """.stripMargin.trim
}
