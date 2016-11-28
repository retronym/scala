package scala.reflect
package internal
package transform

trait PostErasure {
  val global: SymbolTable
  import global._

  object elimErasedValueType extends TypeMap {
    def apply(tp: Type) = tp match {
      case ConstantType(Constant(tp: Type)) => ConstantType(Constant(apply(tp)))
      case ErasedValueType(_, underlying)   => underlying
      case _                                => mapOver(tp)
    }
  }

  // This is confusingly named method, it doesn't implement `InfoTransform#transformInfo`.
  def transformInfo(sym: Symbol, tp: Type) =
    if (sym.isPackageClass || sym.isJavaDefined) tp
    else elimErasedValueType(tp)
}
