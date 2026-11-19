//> using options -feature -language:_ -Wconf:any:s
// Each class has its own pickle, so an unreadable tree only breaks its class.

class ann(x: Any) extends scala.annotation.StaticAnnotation

class Base { def m = 1 }
class Outer { class Inner }
class Dyn extends Dynamic { def selectDynamic(n: String) = n }
object Ex { def unapply(x: Int): Option[Int] = Some(x) }

class T_try_catch { @ann(try 1 catch { case _: Exception => 2 }) def f = 0 }
class T_try_finally { @ann(try 1 finally println()) def f = 0 }
class T_try_catch_finally { @ann(try 1 catch { case e: IllegalStateException => 2; case _: Exception => 3 } finally println()) def f = 0 }
class T_super extends Base { @ann(super.m) def f = 0 }
class T_this { @ann(this) def f = 0 }
class T_local_class { @ann({ class L(val x: Int) { def y = x }; new L(1).y }) def f = 0 }
class T_local_class_var { @ann({ class L { private[this] var z: Int = _; def y = z }; new L().y }) def f = 0 }
class T_self_name { @ann({ class L { self => def y = self.hashCode }; new L().y }) def f = 0 }
class T_self_type { @ann({ trait S; class L { self: S => def y = 1 }; (new L with S).y }) def f = 0 }
class T_local_trait { @ann({ trait Tr { val v: Int }; new Tr { val v = 1 }.v }) def f = 0 }
class T_local_object { @ann({ object O { def x = 1 }; O.x }) def f = 0 }
class T_anon_class { @ann(new Object { override def toString = "anon" }) def f = 0 }
class T_collect { @ann(Seq(1, 2) collect { case x if x > 1 => x }) def f = 0 }
class T_if { @ann(if (System.nanoTime > 0) 1 else 2) def f = 0 }
class T_block { @ann({ val x = 1; x + 1 }) def f = 0 }
class T_function { @ann((x: Int) => x + 1) def f = 0 }
class T_match { @ann((1: Any) match { case 1 | 2 => "a"; case s @ (_: String) if s.length > 0 => s; case _ => "c" }) def f = 0 }
class T_unapply { @ann(1 match { case Ex(y) => y }) def f = 0 }
class T_unapply_seq { @ann(List(1) match { case List(a, _*) => a; case _ => 0 }) def f = 0 }
class T_throw { @ann(throw new RuntimeException("x")) def f = 0 }
class T_new { @ann(new StringBuilder("x")) def f = 0 }
class T_vararg { @ann(List(Seq(1, 2): _*)) def f = 0 }
class T_assign { @ann({ var v = 1; v = 2; v }) def f = 0 }
class T_typed { @ann((1: Any)) def f = 0 }
class T_annotated { @ann((1: @unchecked)) def f = 0 }
class T_local_def { @ann({ def id[T](t: T): T = t; id[Int](1) }) def f = 0 }
class T_local_type { @ann({ type T = Int; (1: T) }) def f = 0 }
class T_dynamic { @ann(new Dyn().foo) def f = 0 }
class T_while { @ann({ var i = 0; while (i < 2) i += 1; i }) def f = 0 }
class T_return { @ann({ def g(): Int = return 1; g() }) def f = 0 }
class T_existential { @ann((null: java.util.List[_])) def f = 0 }
class T_forSome { @ann((null: java.util.List[T] forSome { type T <: AnyRef })) def f = 0 }
class T_refinement { @ann((null: AnyRef { def foo: Int })) def f = 0 }
class T_refinement_val { @ann((null: AnyRef { val foo: Int })) def f = 0 }
class T_applied_type { @ann((null: List[Int])) def f = 0 }
class T_type_projection { @ann((null: Outer#Inner)) def f = 0 }
class T_singleton_type { @ann({ val x = "a"; (x: x.type) }) def f = 0 }
class T_interpolation { @ann(s"a${1}b") def f = 0 }
class T_literals { @ann(List((), true, 1.toByte, 1.toShort, 'c', 1, 1L, 1.5f, 2.5d, "s", null, classOf[String])) def f = 0 }
class T_literal_top { @ann(classOf[String]) def f = 0 }
class T_import { @ann({ import scala.collection.mutable.ListBuffer; new ListBuffer[Int] }) def f = 0 }
class T_import_unused { @ann({ import scala.collection.mutable.ListBuffer; 1 + 1 }) def f = 0 }
class T_import_selectors { @ann({ import scala.collection.mutable.{ListBuffer => LB, ArrayBuffer => _, _}; (new LB[Int], new HashMap[Int, Int]) }) def f = 0 }
class T_import_in_class { @ann({ class L { import scala.collection.mutable._; def y = new ListBuffer[Int] }; new L().y }) def f = 0 }
