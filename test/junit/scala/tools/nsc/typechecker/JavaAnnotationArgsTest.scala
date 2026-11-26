package scala.tools.nsc.typechecker

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import javax.tools.ToolProvider

import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{FileUtils, Global}

/** Annotations of Java definitions parsed from source have the same arguments as when they are read from
 *  classfiles emitted by javac. JLS 9.7.1 allows constant expressions, class literals, enum constants,
 *  annotations and arrays of these as element values.
 */
class JavaAnnotationArgsTest {
  import JavaAnnotationArgsTest._

  val Annots = """
    |import java.lang.annotation.*;
    |@Retention(RetentionPolicy.RUNTIME) @interface P {
    |  boolean z() default false; byte b() default 0; short s() default 0; char c() default 'x';
    |  int i() default 0; long l() default 0; float f() default 0; double d() default 0; String str() default "";
    |}
    |@Retention(RetentionPolicy.RUNTIME) @interface C { Class<?> value() default Object.class; Class<?>[] arr() default {}; }
    |enum E { A, B }
    |@Retention(RetentionPolicy.RUNTIME) @interface Arr {
    |  int[] value() default {}; String[] s() default {}; E[] e() default {}; C[] n() default {}; E en() default E.A; C nested() default @C;
    |}
    |@Retention(RetentionPolicy.RUNTIME) @Target(ElementType.TYPE_PARAMETER) @interface TP { int value(); }
    |class K { static final int N = 3; static final String S = "k"; static final char CH = 'q'; }
    |""".stripMargin

  val J = """
    |import java.util.Map;
    |@P(z = true, b = 1, s = 2, c = 'c', i = 4, l = 5L, f = 6.5f, d = 7.25, str = "s")
    |@C(int.class)
    |@Arr({1, 2, 3,})
    |public class J<@TP(1 + 1) T> {
    |  static final int M = 10;
    |  static final String MS = "m";
    |
    |  @P(i = 0x1F + 0b101 + 017 + 1_000, l = -9223372036854775808L, s = -32768, b = (byte) 300, c = (char) 65,
    |     f = 16777217, d = 1 / 2, str = "a" + 1 + 'c' + 1.5f + 2.5 + true + 10L)
    |  public int literals;
    |
    |  @P(i = -2147483648, l = 1L << 40, z = !false, b = 10 + 20, s = (short) -70000, c = 'a' + 1, f = 1.0f / 3, d = 5.5 % 2)
    |  public int literals2;
    |
    |  @P(i = (1 + 2) * 3 - 4 / 2 % 3, l = ~0L ^ 5L | 6L & 7L, z = 1 < 2 && 2 <= 3 || 3 > 4 == 4 >= 5 != true)
    |  public void arithmetic() { }
    |
    |  @P(i = -16 >> 2, l = -1L >>> 63, s = 1 << 3, z = true ^ true, d = +1.0, f = -(float) 2, c = (char) ('a' + 2))
    |  public void shifts() { }
    |
    |  @P(i = M > 0 ? 1 : 2, c = true ? 'x' : 0, str = M > 5 ? "big" : "small", z = "a" == "a")
    |  public void conditional() { }
    |
    |  @P(i = M + K.N + J.M, str = MS + K.S + (String) "x" + (java.lang.String) "y" + (char) 65 + (1 + 2), c = K.CH,
    |     l = Integer.MAX_VALUE + 1L, d = Double.MAX_VALUE, b = Byte.MIN_VALUE)
    |  public void references() { }
    |
    |  @C(value = void.class, arr = {int.class, boolean.class, String[].class, int[][].class, Map.Entry.class, Map.Entry[].class, java.lang.String.class, J.class})
    |  public J(@C(String[].class) int x, @P(i = (int) 2L) int y) { }
    |
    |  @Arr(value = {}, s = {"a" + "b", MS,}, e = {E.A, E.B}, n = {@C(long.class), @C(arr = {})}, en = E.B, nested = @C(double[].class))
    |  public int arrays;
    |
    |  @Arr(value = 1 + 1, s = "single", e = E.B, n = @C(byte.class))
    |  public int singleElement;
    |
    |  @Arr((1)) @P(i = (int) 2L, l = (1), s = (short) (1 + 2) * (3), d = -(1), str = ("p"))
    |  public int parenthesised;
    |
    |  @Arr(value = {(1), (int) 2L}) @P(str = (String) ("q" + 1))
    |  public void <X> parenthesisedArray(@P(i = (1)) int p, @Arr({(1), 2}) int q) { }
    |
    |  @Deprecated public static class Inner<@TP(2) U> { @C(Inner.class) public U u; }
    |}
    |""".stripMargin.replace("public void <X>", "public <X> void")

  @Test def annotationArgsFromSourceMatchClassfile(): Unit = {
    val sources = List("Annots.java" -> Annots, "J.java" -> J)
    val classes = List("J", "J.Inner")
    val expected = javacAnnotations(sources, classes)
    assertTrue(expected.mkString("\n"), expected.exists(_.contains("classOf[scala.Unit]")))
    val actual = annotations(JavaConstantsTest.newGlobal(""), sources.map { case (n, s) => new BatchSourceFile(n, s) }, classes, params = true)
    assertEquals(expected.mkString("\n"), actual.filterNot(isParam).mkString("\n"))
    // the classfile parser does not read the annotations of parameters and type parameters
    assertEquals(
      """J.<init>(x): C(value = classOf[[Ljava.lang.String;])
        |J.<init>(y): P(i = 2)
        |J.Inner[U]: TP(value = 2)
        |J.parenthesisedArray(p): P(i = 1)
        |J.parenthesisedArray(q): Arr(value = [1, 2])
        |J[T]: TP(value = 2)""".stripMargin,
      actual.filter(isParam).mkString("\n"))
  }

  /** An argument that is not a valid element value drops the annotation, and the rest of the file is parsed. */
  @Test def invalidArgDropsAnnotation(): Unit = {
    val src = """
      |@C(Object.class) public class V {
      |  @P(i = Integer.parseInt("1")) @C(String.class) public int call;
      |  @P(i = new int[]{1, (2)}.length) @C(String.class) public int newArray;
      |  @P(i = ((1)) + foo((1), {2})) @C(String.class) public int nested;
      |  @Arr({1, x.y(), 3}) @C(String.class) public int inArray;
      |  public static final int[] ARR = {1};
      |  public static final int ELEM = ARR[0];
      |  @P(i = ARR[0]) @C(String.class) public int index;
      |  @C(Integer.class) public int after;
      |}
      |""".stripMargin
    val g = JavaConstantsTest.newGlobal("")
    val actual = annotations(g, List(new BatchSourceFile("Annots.java", Annots), new BatchSourceFile("V.java", src)), List("V"))
    assertEquals(
      """V.after: C(value = classOf[java.lang.Integer])
        |V.call: C(value = classOf[java.lang.String])
        |V.inArray: C(value = classOf[java.lang.String])
        |V.index: C(value = classOf[java.lang.String])
        |V.nested: C(value = classOf[java.lang.String])
        |V.newArray: C(value = classOf[java.lang.String])
        |V: C(value = classOf[java.lang.Object])""".stripMargin,
      actual.mkString("\n"))
  }
}

object JavaAnnotationArgsTest {
  def isParam(line: String) = line.contains("(") && line.indexOf('(') < line.indexOf(':') || line.contains("]:")

  /** The annotations of the given classes and their members, and with `params` their parameters and type parameters,
   *  as seen after typer.
   */
  def annotations(g: Global, javaSources: List[SourceFile], classes: List[String], params: Boolean = false): List[String] = {
    import g._, rootMirror.EmptyPackageClass
    val run = new Run
    run.compileSources(javaSources)
    assert(!reporter.hasErrors, reporter.asInstanceOf[StoreReporter].infos.mkString("\n"))
    // the classfile parser reads `boolean` and `char` element values as `int` constants
    def showArg(arg: ClassfileAnnotArg): String = arg match {
      case LiteralAnnotArg(c) if c.tag == BooleanTag => if (c.booleanValue) "1" else "0"
      case LiteralAnnotArg(c) if c.tag == CharTag    => c.intValue.toString
      case ArrayAnnotArg(args)                       => args.map(showArg).mkString("[", ", ", "]")
      case NestedAnnotArg(ann)                       => showAnnot(ann)
      case _                                         => arg.toString
    }
    def showAnnot(ann: AnnotationInfo): String =
      if (ann.assocs.isEmpty) ann.toString
      else ann.assocs.map { case (n, a) => s"$n = ${showArg(a)}" }.mkString(s"${ann.atp}(", ", ", ")")
    def show(prefix: String, sym: Symbol): List[String] = {
      sym.info // adds the annotations of a symbol from source
      sym.annotations.foreach(_.completeInfo())
      val annots = sym.annotations.filter(_.atp != NoType) // not `UnmappableAnnotation`
      val own = if (annots.isEmpty) Nil else List(s"$prefix: ${annots.map(showAnnot).mkString(" ")}")
      own ::: (if (params) sym.typeParams.flatMap(tp => show(s"$prefix[${tp.name}]", tp)) else Nil)
    }
    exitingTyper {
      for {
        cls  <- classes
        sym  <- {
          val path = cls.split('.')
          val top = EmptyPackageClass.info.decl(TypeName(path.head))
          val c = path.tail.foldLeft(top)((owner, n) => owner.info.decl(TypeName(n)).orElse(owner.companionModule.moduleClass.info.decl(TypeName(n))))
          List(c, c.companionModule.moduleClass)
        }
        line <- show(cls, sym) ::: sym.info.decls.toList.flatMap { decl =>
          val name = s"$cls.${decl.name.decoded}"
          show(name, decl) ::: (if (params) decl.paramss.flatten.flatMap(p => show(s"$name(${p.name})", p)) else Nil)
        }
      } yield line
    }.sorted.distinct
  }

  /** The annotations of the given classes, read from the classfiles that javac emits for `sources`. */
  def javacAnnotations(sources: List[(String, String)], classes: List[String]): List[String] = {
    val out = Files.createTempDirectory("javac-out")
    try {
      val srcs = sources.map { case (n, s) => Files.write(out.resolve(n), s.getBytes(UTF_8)).toString }
      val err = new java.io.ByteArrayOutputStream
      val status = ToolProvider.getSystemJavaCompiler.run(null, null, err, ("-encoding" :: "UTF-8" :: "-nowarn" :: "-parameters" :: "-d" :: out.toString :: srcs): _*)
      assertEquals(err.toString, 0, status)
      srcs.foreach(s => Files.delete(java.nio.file.Paths.get(s)))
      annotations(JavaConstantsTest.newGlobal(out.toString), Nil, classes)
    } finally FileUtils.deleteRecursive(out)
  }
}
