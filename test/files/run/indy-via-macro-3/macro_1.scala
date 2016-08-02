import java.util.regex.{Pattern, PatternSyntaxException}

import scala.language.experimental.macros
import scala.reflect.internal.SymbolTable
import scala.reflect.macros.blackbox._

object Macro {
  def compareAndSetInt(cls: Class[_], field: String, value: AnyRef, expect: Int, update: Int): Boolean = macro impl.compareAndSetInt

  class impl(val c: Context) extends IndySupport {
    import c.universe._, c.internal.reificationSupport.MethodType

    val boostrapSym = typeOf[test.Bootstrap].companion.member(TermName("bootstrap"))
    assert(boostrapSym != NoSymbol)
    val invokedType = newMethodType(List(typeOf[Object], typeOf[Int], typeOf[Int]), typeOf[Boolean])

    def compareAndSetInt(cls: Tree, field: Tree, value: Tree, expect: Tree, update: Tree): Tree = {
      (cls, field) match {
        case (c@Literal(Constant(cValue: Type)), l@Literal(Constant(_: String))) =>
          Indy(boostrapSym, List(c, l), List(value, expect, update), invokedType)
        case _ =>
          c.abort(c.macroApplication.pos, "cls and field params must be literals")
      }
    }
  }
}

trait IndySupport {
  val c: Context
  import c.universe._
  def newMethodType(paramTps: List[Type], resTp: Type): MethodType = {
    val symtab = c.universe.asInstanceOf[SymbolTable]
    val paramTps1 = paramTps.asInstanceOf[List[symtab.Type]]
    val resTp1 = resTp.asInstanceOf[symtab.Type]
    val params1 = paramTps1.zipWithIndex.map {
      case (tp, i) =>
        val param = symtab.NoSymbol.newValueParameter(symtab.TermName("param$" + i), c.macroApplication.pos.focus.asInstanceOf[symtab.Position])
        param.setInfo(tp)
    }
    symtab.MethodType(params1, resTp1).asInstanceOf[c.universe.MethodType]
  }

  def Indy(bootstrapMethod: Symbol, bootstrapArgs: List[Literal], dynArgs: List[Tree], invokedType: Type, name: TermName = TermName("dummy")): Tree = {
    val symtab = c.universe.asInstanceOf[SymbolTable]
    val result = {
      import symtab._
      val dummySymbol = NoSymbol.newTermSymbol(name.asInstanceOf[symtab.TermName]).setInfo(invokedType.asInstanceOf[symtab.Type])
      val args: List[Tree] = Literal(Constant(bootstrapMethod)).setType(NoType) :: bootstrapArgs.asInstanceOf[List[Tree]]
      ApplyDynamic(Ident(dummySymbol).setType(dummySymbol.info), args ::: dynArgs.asInstanceOf[List[Tree]]).setType(invokedType.resultType.asInstanceOf[symtab.Type])
    }
    result.asInstanceOf[Tree]
  }

}
