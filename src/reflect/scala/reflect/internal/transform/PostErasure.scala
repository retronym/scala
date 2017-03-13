package scala.reflect
package internal
package transform

trait PostErasure {
  val global: SymbolTable
  import global._

  object elimErasedValueType extends TypeMap {
    def apply(tp: Type) = tp match {
      case ConstantType(Constant(tpVal: Type)) =>
        val tpVal1 = apply(tpVal)
        if (tpVal1 eq tpVal) tp
        else ConstantType(Constant(tpVal1))
      case ErasedValueType(_, underlying)   => underlying
      case _                                => mapOver(tp)
    }
  }

  def transformInfo(sym: Symbol, tp: Type) = elimErasedValueType(tp)
}
