import reflect.macros.Context, language.experimental._

object OK {
  def impl[A: c.WeakTypeTag](c: Context)(x: c.Expr[A]) =
    c.universe.reify( () => c.Expr[A](c.resetLocalAttrs(x.tree)).splice )

  def m[A](x: A) = macro impl[A]
}

object KO {
  // Can we detect the failure to reset and/or the consequent messed up owner chain?
  def impl[A: c.WeakTypeTag](c: Context)(style: c.Expr[Int])(x: c.Expr[A]): c.Expr[Any] = {
    import c.universe._
    val Literal(Constant(styleInt: Int)) = style.tree
    styleInt match {
      case 0 => reify { () => x.splice }
      case 1 => reify { def foo = x.splice; foo }
      case 2 => reify { val foo = x.splice; foo }
      case 3 => reify { new { x.splice } }
      case 4 => reify { object O { x.splice } }
      case 5 => reify { class C { x.splice }; }
    }
  }

  def m[A](style: Int)(x: A): Any = macro impl[A]
}
