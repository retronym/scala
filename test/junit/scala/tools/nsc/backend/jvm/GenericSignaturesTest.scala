package scala.tools.nsc.backend.jvm

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.jdk.CollectionConverters._
import scala.tools.testkit.BytecodeTesting

@RunWith(classOf[JUnit4])
class GenericSignaturesTest extends BytecodeTesting {
  import compiler._

  @Test
  def nestedModules(): Unit = {
    val code =
      """class C[T] {
        |  object O {
        |    object I
        |    class J[U]
        |    class K[V] extends J[V]
        |  }
        |}
      """.stripMargin
    val List(c, o, i, j, k) = compileClasses(code)
    assertEquals(o.name, "C$O$")
    assertEquals(o.methods.asScala.find(_.name == "I").get.signature, "()LC<TT;>.O$I$;")
    // `T` is erased: javac cannot resolve it from a class nested in the module class `C$O$`
    assertEquals(k.signature, "<V:Ljava/lang/Object;>LC<Ljava/lang/Object;>.O$J<TV;>;")
  }

  @Test
  def t10351(): Unit = {
    val code =
      """trait A[U] {
        |  type B <: U
        |}
        |class C {
        |  val a: A[Int] = ???
        |  val b: a.B = ???
        |}
      """.stripMargin

    val List(_, c) = compileClasses(code)
    assertEquals(
      List(("a", "LA<Ljava/lang/Object;>;"), ("b", null)),
      c.fields.asScala.toList.map(f => (f.name, f.signature)).sorted)
  }

  @Test
  def t9810(): Unit = {
    val code =
      """class A[+P] (final val id: Int) extends AnyVal
        |class C extends AnyRef
        |object C {
        |  final val key: A[C] = new A(1)
        |}
      """.stripMargin
    val List(a, aM, c, cM) = compileClasses(code)
    assertEquals(List(("MODULE$", null), ("key", null)),
      cM.fields.asScala.toList.map(f => (f.name, f.signature)).sorted)
  }

  /** The erasure of a method's generic signature as javac computes it: type arguments are dropped
   *  and type variables are replaced by `Object` (the tests that use this have no bounded ones).
   */
  private def eraseSignature(sig: String): String = {
    val sb = new StringBuilder
    var i = 0
    def skipTypeArgs(): Unit = { // at '<'
      var depth = 0
      while ({ if (sig(i) == '<') depth += 1 else if (sig(i) == '>') depth -= 1; i += 1; depth > 0 }) ()
    }
    def tpe(): Unit = sig(i) match {
      case '[' => sb.append('['); i += 1; tpe()
      case 'T' => i = sig.indexOf(';', i) + 1; sb.append("Ljava/lang/Object;")
      case 'L' =>
        while (sig(i) != ';') sig(i) match {
          case '<' => skipTypeArgs()
          case '.' => sb.append('$'); i += 1
          case c   => sb.append(c); i += 1
        }
        sb.append(';'); i += 1
      case c => sb.append(c); i += 1 // base type or V
    }
    if (sig(i) == '<') skipTypeArgs()
    assert(sig(i) == '(', sig)
    sb.append('('); i += 1
    while (sig(i) != ')') tpe()
    sb.append(')'); i += 1
    tpe()
    sb.toString
  }

  @Test
  def arrayElementSignatures(): Unit = {
    val code =
      """class VG[T](val t: T) extends AnyVal
        |class VI(val i: Int) extends AnyVal
        |class C {
        |  def a(x: Array[VG[String]]) = x
        |  def b(x: List[Array[VG[String]]]) = x
        |  def c[T](x: Array[VG[T]]) = x
        |  def d(x: Array[VI], l: List[String]) = x
        |  def e(x: Array[Array[VI]], l: List[String]) = x
        |  def n[T](x: Array[Nothing], t: T): Array[Null] = null
        |  def nn[T](x: Array[Array[Nothing]], t: T): Array[Array[Null]] = null
        |  def l(x: List[Array[Nothing]]): List[Array[Null]] = null
        |  def p(x: Array[Int], l: List[String]): Array[Array[Int]] = null
        |  def g[T](x: Array[T]): Array[Array[T]] = null
        |  def s(x: Array[String], l: List[String]): Array[Array[String]] = null
        |  val f: Array[VG[Int]] = null
        |}
      """.stripMargin
    val c = compileClasses(code).find(_.name == "C").get
    val methods = c.methods.asScala.toList.filter(_.signature != null).map(m => (m.name, m.desc, m.signature)).sortBy(_._1)
    for ((name, desc, sig) <- methods)
      assertEquals(s"erased Signature of $name", desc, eraseSignature(sig))
    assertEquals(List(
      "a"  -> "([LVG<Ljava/lang/String;>;)[LVG<Ljava/lang/String;>;",
      "b"  -> "(Lscala/collection/immutable/List<[LVG<Ljava/lang/String;>;>;)Lscala/collection/immutable/List<[LVG<Ljava/lang/String;>;>;",
      "c"  -> "<T:Ljava/lang/Object;>([LVG<TT;>;)[LVG<TT;>;",
      "d"  -> "([LVI;Lscala/collection/immutable/List<Ljava/lang/String;>;)[LVI;",
      "e"  -> "([[LVI;Lscala/collection/immutable/List<Ljava/lang/String;>;)[[LVI;",
      "f"  -> "()[LVG<Ljava/lang/Object;>;",
      "g"  -> "<T:Ljava/lang/Object;>(Ljava/lang/Object;)[Ljava/lang/Object;",
      "l"  -> "(Lscala/collection/immutable/List<[Ljava/lang/Object;>;)Lscala/collection/immutable/List<[Ljava/lang/Object;>;",
      "n"  -> "<T:Ljava/lang/Object;>([Ljava/lang/Object;TT;)[Ljava/lang/Object;",
      "nn" -> "<T:Ljava/lang/Object;>([[Ljava/lang/Object;TT;)[[Ljava/lang/Object;",
      "p"  -> "([ILscala/collection/immutable/List<Ljava/lang/String;>;)[[I",
      "s"  -> "([Ljava/lang/String;Lscala/collection/immutable/List<Ljava/lang/String;>;)[[Ljava/lang/String;",
    ), methods.map(m => (m._1, m._3)))
  }
}
