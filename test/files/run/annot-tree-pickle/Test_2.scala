import scala.reflect.runtime.universe._

object Test {
  // Instantiating each class makes the compiler unpickle its signature, annotations included.
  def compileTimeUse = List(
    new T_try_catch, new T_try_finally, new T_try_catch_finally, new T_super, new T_this,
    new T_local_class, new T_local_class_var, new T_self_name, new T_self_type, new T_local_trait,
    new T_local_object, new T_anon_class, new T_collect, new T_if, new T_block, new T_function,
    new T_match, new T_unapply, new T_unapply_seq, new T_throw, new T_new, new T_vararg, new T_assign,
    new T_typed, new T_annotated, new T_local_def, new T_local_type, new T_dynamic, new T_while,
    new T_return, new T_existential, new T_forSome, new T_refinement, new T_refinement_val,
    new T_applied_type, new T_type_projection, new T_singleton_type, new T_interpolation,
    new T_literals, new T_literal_top, new T_import, new T_import_unused, new T_import_selectors,
    new T_import_in_class)

  def main(args: Array[String]): Unit = {
    val m = runtimeMirror(getClass.getClassLoader)
    for (c <- compileTimeUse) {
      val cls = m.classSymbol(c.getClass)
      val res =
        try cls.info.decl(TermName("f")).annotations.map(a => a.tree.children.tail.map(show(_)).mkString(", ")).mkString("; ")
        catch { case t: Throwable => s"${t.getClass.getName}: ${t.getMessage}" }
      println(s"${cls.name}: $res")
    }
  }
}
