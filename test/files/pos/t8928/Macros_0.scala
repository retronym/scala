package test

object Macros {
  import language.experimental.macros
  def apply[T]: Unit = macro impl[T]

  import reflect.macros.whitebox
  def impl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val ObjectTpe = typeOf[Object]

    def assertMatch(v: Tree)(pf: PartialFunction[Tree, Any]): Unit =
      if (!pf.isDefinedAt(v)) c.abort(v.pos, showRaw(v))

    val anns = symbolOf[T].annotations

    anns match {
      case List(noArgs, simple, nested, arrays, enum) =>
        assert(noArgs.tree.tpe =:= typeOf[NoArgs_0])
        assert(noArgs.tree.children.size == 1)

        assert(simple.tree.tpe =:= typeOf[Simple_0])
        simple.tree.children match {
          case _ :: byte :: char :: short :: int :: long :: float :: double :: string :: clasz :: Nil =>
            assertMatch(byte)  { case AssignOrNamedArg(Ident(TermName("_byte")),   Literal(Constant(1))) =>           }
            assertMatch(char)  { case AssignOrNamedArg(Ident(TermName("_char")),   Literal(Constant('2'))) =>         }
            assertMatch(short) { case AssignOrNamedArg(Ident(TermName("_short")),  Literal(Constant(3))) =>           }
            assertMatch(int)   { case AssignOrNamedArg(Ident(TermName("_int")),    Literal(Constant(4))) =>           }
            assertMatch(long)  { case AssignOrNamedArg(Ident(TermName("_long")),   Literal(Constant(5L))) =>          }
            assertMatch(float) { case AssignOrNamedArg(Ident(TermName("_float")),  Literal(Constant(6.7f))) =>        }
            assertMatch(double){ case AssignOrNamedArg(Ident(TermName("_double")), Literal(Constant(8.9d))) =>        }
            assertMatch(string){ case AssignOrNamedArg(Ident(TermName("_string")), Literal(Constant("ten"))) =>       }
            assertMatch(clasz) { case AssignOrNamedArg(Ident(TermName("_class")),  Literal(Constant(ObjectTpe))) =>   }
        }

        assert(nested.tree.tpe =:= typeOf[Nested_0])
        nested.tree.children match {
          case _ :: inner :: Nil =>
            assertMatch(inner) {
              case AssignOrNamedArg(Ident(TermName("inner")), Apply(Select(New(tpe), nme.CONSTRUCTOR), AssignOrNamedArg(Ident(TermName("value")), Literal(Constant("turkey"))) :: Nil))
                if tpe.tpe =:= typeOf[Nested_0.Inner] =>
            }
        }

        assert(arrays.tree.tpe =:= typeOf[Array_0.Repeated])
        arrays.tree.children match {
          case _ :: AssignOrNamedArg(Ident(TermName("value")), Apply(arr, fst :: snd :: Nil)) :: Nil =>
            assertMatch(fst) {
              case Apply(Select(New(tpe), nme.CONSTRUCTOR), AssignOrNamedArg(Ident(TermName("value")), Apply(arr, args)) :: Nil)
                if ((args zip Seq(8, 6, 7, 5, 3, 0, 9)) forall { case (Literal(Constant(l)), r) => l == r }) &&
                  tpe.tpe =:= typeOf[Array_0] =>
            }
            assertMatch(snd) {
              case Apply(Select(New(tpe), nme.CONSTRUCTOR), AssignOrNamedArg(Ident(TermName("value")), Apply(arr, args)) :: Nil)
                if ((args zip Seq(6)) forall { case (Literal(Constant(l)), r) => l == r }) &&
                  tpe.tpe =:= typeOf[Array_0] =>
            }
        }
        assert(enum.tree.tpe =:= typeOf[Enum_0])
    }

    Literal(Constant(()))
  }
}