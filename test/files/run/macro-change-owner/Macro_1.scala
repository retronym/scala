import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

class Impl(val c: Context) {
  import c._
  import c.universe._
  private def markForChangeOwner(t: Tree): t.type = {
    val symtab = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
    // t.asInstanceOf[symtab.Tree].updateAttachment(
    //   symtab.ChangeOwnerAttachment(c.internal.enclosingOwner.asInstanceOf[symtab.Symbol])
    // )
    t
  }
  
  def valDef[T: c.WeakTypeTag](t: Tree): Tree = {
    val Block(stat :: Nil, expr) = t
    q"$stat; class X { val newVal = $expr}; new X().newVal"
  }

  def defDef[T: c.WeakTypeTag](t: Tree): Tree = {
    val Block(stat :: Nil, expr) = t
    q"$stat; class X { def newDef = $expr}; new X().newDef"
  }

  def lambda[T: c.WeakTypeTag](t: Tree): Tree = {
    val Block(stat :: Nil, expr) = t
    q"$stat; class X { val newLambda = () => $expr}; new X().newLambda()"
  }
}

object Macro {
  def valDef[T](t: T): T = macro Impl.valDef[T]
  def defDef[T](t: T): T = macro Impl.defDef[T]
  def lambda[T](t: T): T = macro Impl.lambda[T]
}
