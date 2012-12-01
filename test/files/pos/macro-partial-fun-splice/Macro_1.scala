import reflect.macros.Context
import language.experimental._

object Macros {
	def impl(c: Context)(expr: c.Expr[Any]): c.Expr[Any] = {
		import c.universe._

		reify {
			() => c.Expr[Any](c.resetLocalAttrs(expr.tree)).splice
    }
	}

	def m(expr: Any) = macro impl
}
