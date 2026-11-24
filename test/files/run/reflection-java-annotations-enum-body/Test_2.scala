object Test extends App {
  import scala.reflect.runtime.universe._
  val sym = typeOf[Annottee_1].typeSymbol
  sym.info
  sym.annotations foreach (_.tree.children.tail)
  println(sym.annotations)
  sym.annotations.map(_.tree).foreach(println)
  // An enum constant with a body is an instance of an anonymous subclass of the enum.
  // Reflecting the annotation must not enter that subclass as a member of the enum.
  val enumSym = typeOf[E_1].typeSymbol
  println(enumSym.info.decls.sorted.map(_.toString))
  println(enumSym.companion.info.decls.sorted.filter(_.isJavaEnum).map(_.toString))
}
