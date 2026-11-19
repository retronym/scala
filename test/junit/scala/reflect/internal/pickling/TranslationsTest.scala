package scala.reflect.internal.pickling

import org.junit.Assert._
import org.junit.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

class TranslationsTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._

  /** The pickler writes a symbol for a tree iff `hasSymbolField`; the unpickler decides by tag. */
  @Test def treeSymbolPickledIffHasSymbolField(): Unit = {
    val id = Ident(TermName("x"))
    val tid = Ident(TypeName("T"))
    val vd = ValDef(NoMods, TermName("x"), TypeTree(), EmptyTree)
    val td = TypeDef(NoMods, TypeName("T"), Nil, TypeTree())
    val templ = Template(Nil, noSelfType, Nil)
    val cd = CaseDef(id, EmptyTree, id)
    val trees = List[Tree](
      PackageDef(id, Nil), ClassDef(NoMods, TypeName("C"), Nil, templ), ModuleDef(NoMods, TermName("O"), templ), vd,
      DefDef(NoMods, TermName("f"), Nil, Nil, TypeTree(), id), td, LabelDef(TermName("l"), Nil, id),
      Import(id, Nil), templ, Block(Nil, id), cd, Alternative(Nil), Star(id), Bind(TermName("b"), id),
      UnApply(id, Nil), ArrayValue(TypeTree(), Nil), Function(Nil, id), Assign(id, id), If(id, id, id), Match(id, Nil),
      Return(id), Try(id, Nil, EmptyTree), Throw(id), New(tid), Typed(id, tid), TypeApply(id, Nil), Apply(id, Nil),
      ApplyDynamic(id, Nil), Super(This(TypeName("C")), TypeName("")), This(TypeName("C")), Select(id, TermName("y")), id,
      Literal(Constant(1)), TypeTree(), Annotated(id, id), SingletonTypeTree(id), SelectFromTypeTree(tid, TypeName("U")),
      CompoundTypeTree(templ), AppliedTypeTree(tid, Nil), TypeBoundsTree(tid, tid), ExistentialTypeTree(tid, Nil))
    for (t <- trees)
      assertEquals(t.shortClass, t.hasSymbolField, translations.isTreeSymbolPickled(t))
  }
}
