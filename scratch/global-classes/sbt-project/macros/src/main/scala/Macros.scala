import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Macros {
  // Like jsoniter-scala's JsonCodecMaker: evaluates a constant argument at compile time with c.eval,
  // which creates a ToolBoxGlobal.
  def evalConst(x: Int): Int = macro evalConstImpl
  def evalConstImpl(c: blackbox.Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    val v = c.eval(c.Expr[Int](c.untypecheck(x.tree.duplicate)))
    c.Expr[Int](Literal(Constant(v)))
  }
}
