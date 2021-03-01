import java.util.regex._

import scala.reflect.internal.SymbolTable
import scala.reflect.macros.blackbox._
import language.experimental.macros
import java.lang.invoke.MethodHandle

object Macro {

  def reflector(dynamic: String): String = macro Macro.impl
  def impl(c: Context)(dynamic: c.Tree): c.Tree = {
    {
      val symtab = c.universe.asInstanceOf[SymbolTable]
      import symtab._
      val bootstrapMethod = typeOf[test.Bootstrap].companion.member(TermName("bootstrap"))
      val paramSym = NoSymbol.newTermSymbol(TermName("x")).setInfo(typeOf[String])
      val dummySymbol                   = NoSymbol.newTermSymbol(TermName("reflector")).setInfo(internal.methodType(paramSym :: Nil, typeOf[String]))
      val reflectionSubject = c.internal.enclosingOwner.asInstanceOf[symtab.Symbol]
      val reflectionSubjectParams = reflectionSubject.info.paramss.flatten
      val bootstrapArgTrees: List[Tree] = List(
        Literal(Constant(bootstrapMethod)).setType(NoType),
        Literal(Constant(reflectionSubjectParams.length)).setType(typeOf[Int]),
        Literal(Constant(reflectionSubject)).setType(typeOf[MethodHandle])
      ) ::: reflectionSubjectParams.map(s => Literal(Constant(s.name.decoded)).setType(typeOf[String]))
      val result                        = ApplyDynamic(Ident(dummySymbol).setType(dummySymbol.info), bootstrapArgTrees ::: List(dynamic.asInstanceOf[symtab.Tree]))
      result.setType(dummySymbol.info.resultType)
      result.asInstanceOf[c.Tree]
    }
  }
}
