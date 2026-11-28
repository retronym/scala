import scala.language.experimental.macros
import scala.reflect.macros.blackbox
object Annots {
  /** The annotations the compiler sees on the named class, as a string computed at compile time. */
  def of(name: String): String = macro impl
  def impl(c: blackbox.Context)(name: c.Expr[String]): c.Expr[String] = {
    import c.universe._
    val Literal(Constant(n: String)) = name.tree
    val s = c.mirror.staticClass(n)
    s.info
    c.Expr[String](Literal(Constant(n + ": " + s.annotations.map { a => a.tree; a.toString }.mkString(", "))))
  }
}
