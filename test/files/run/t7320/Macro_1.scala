import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def demo(t: Seq[String => String]): List[Seq[String => String]] = macro macroImpl

  def macroImpl(c: Context)(t: c.Expr[Seq[String => String]]): c.Expr[List[Seq[String => String]]] = {
    val r = c.universe.reify { List(t.splice) }
    c.Expr[List[Seq[String => String]]]( c.resetLocalAttrs(r.tree) )
  }
}
