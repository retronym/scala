//> using options -Ymacro-annotations
import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.annotation.StaticAnnotation

object Macros {
  def identImpl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    c.Expr[Any](q"{..${annottees.map(_.tree)}}")
  }
}

class ident extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macros.identImpl
}
