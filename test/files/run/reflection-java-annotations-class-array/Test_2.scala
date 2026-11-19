object Test extends App {
  import scala.reflect.runtime.universe._
  val sym = typeOf[Annottee_1].typeSymbol
  sym.info
  sym.annotations foreach (_.tree.children.tail)
  val annotArgs = sym.annotations.head.tree.children.tail
  annotArgs.foreach(println)
  def show(arg: Tree): String = arg match {
    case Literal(Constant(tpe: Type))  => tpe.toString
    case Apply(_, elems)               => elems.map(show).mkString("{", ", ", "}")
    case x                             => throw new MatchError(x)
  }
  annotArgs.foreach { case NamedArg(name, arg) => println(s"$name: ${show(arg)}"); case x => throw new MatchError(x) }
}
