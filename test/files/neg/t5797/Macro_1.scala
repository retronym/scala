import reflect.macros.Context, language.experimental._

object OK {
  def impl[A: c.WeakTypeTag](c: Context)(x: c.Expr[A]) =
    c.universe.reify( () => c.Expr[A](c.resetLocalAttrs(x.tree)).splice )

  def m[A](x: A) = macro impl[A]
}

object KO {
  // Can we detect the failure to reset and/or the consequent messed up owner chain?
  def impl[A: c.WeakTypeTag](c: Context)(x: c.Expr[A]) =
    c.universe.reify( () => x.splice )

  def m[A](x: A) = macro impl[A]
}
