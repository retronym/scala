package scala.tools.nsc
package async

import java.io.File
import java.nio.file.{Files, Paths}

import org.junit.Assert.assertEquals
import org.junit.{Assert, Ignore, Test}

import scala.annotation.StaticAnnotation
import scala.concurrent.duration.Duration
import scala.reflect.internal.SymbolTable
import scala.reflect.internal.util.Position
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.async.StateAssigner
import scala.tools.nsc.transform.async.user.FutureSystem

class AnnotationDrivenAsync {
  @Test
  def testBasicScalaConcurrent(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.async.Async.{async, await}
        |
        |object Test {
        |  def test: Future[Int] = async { await(f(1)) + await(f(2)) }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(3, run(code))
  }

  @Test
  def testBasicScalaConcurrentValueClass(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.async.Async.{async, await}
        |import Future.{successful => f}
        |
        |class IntWrapper(val value: String) extends AnyVal
        |object Test {
        |  def test: Future[String] = async { await(inner).value }
        |  def inner: Future[IntWrapper] = async { await(f(new IntWrapper("hola"))) }
        |}
        |""".stripMargin
    assertEquals("hola", run(code))
  }

  @Test
  def testMatchBig(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.async.Async.{async, await}
        |
        |
        |object Test {
        |  def test: Future[Int] = async {
        |    val x: Option[Either[Object, (String, String)]] = Some(Right(("a", "b")))
        |    x match {
        |      case Some(Left(_)) => 1
        |      case Some(Right(("a", "c"))) => 2
        |      case Some(Right(("a", "e"))) => 3
        |      case Some(Right(("a", x))) if "ab".isEmpty => 4
        |      case Some(Right(("a", "b"))) => await(f(5))
        |      case Some(Right((y, x))) if x == y => 6
        |      case Some(Right((_, _))) => await(f(7))
        |      case None => 8
        |    }
        | }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(5, run(code))
  }
 @Test
  def testMatchSmall(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.async.Async.{async, await}
        |
        |
        |object Test {
        |  def test: Future[Int] = async {
        |    val x: Option[Either[Object, (String, String)]] = Some(Right(("a", "b")))
        |    (x: @unchecked) match {
        |      case Some(Right(("a", "b"))) => await(f(5))
        |      case None => 8
        |    }
        | }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(5, run(code))
  }


  @Test
  def testBasicScalaConcurrentCapture(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.async.Async.{async, await}
        |
        |object Test {
        |  def test: Future[(String, Int, Int)] = async { var x = "init"; val y = await(f(1)); class C { x = x + "_updated" }; new C; (x, y, await(f(2))) }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(("init_updated", 1, 2), run(code))
  }

 @Test
  def testLiftedLazyVal(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.async.Async.{async, await}
        |
        |object Test {
        |  def test: Future[(Int, Int, Int)] = async { var i = 0; var z = 0; lazy val foo = { def ii = i; z = -1; ii }; await(f(1)) + await(f(2)); i += 1; (foo, foo, z) }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals((1, 1, -1), run(code))
  }

  @Test
  def testWhile1(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.async.Async.{async, await}
        |
        |object Test {
        |  def p[T](t: T): T = {println(t); t }
        |  def test: Future[Int] = async {
        |    var sum = 0
        |    var i = 0
        |    while (i < 5) {
        |      var j = 0
        |      while (j < 5) {
        |        sum += await(f(i)) * await(f(j))
        |        j += 1
        |      }
        |      i += 1
        |    }
        |    sum
        |  }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(100, run(code))
  }

  @Test
  def testWhile2(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.async.Async.{async, await}
        |
        |object Test {
        |  def p[T](t: T): T = {println(t); t }
        |  def test: Future[Int] = async {
        |    var sum = 0
        |    var i = 0
        |    while (i < 5) {
        |      sum += await(f(i))
        |      i += 1
        |    }
        |    sum
        |  }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(10, run(code))
  }

  @Test
  def testCaseClassLifting(): Unit = {
    // Note: this emits a warning under -Ydebug (which we sometimes manually set below in the compiler setup)
    // https://github.com/scala/scala/pull/8750 will fix this.
    val code =
      """import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
         import scala.async.Async.{async, await}
         import Future.{successful => f}
         object Test {
           def test = async {
             {
              trait Base { def base = 0}
              await(f(0))
              case class Person(name: String) extends Base
              val fut = f("bob")
              val x = Person(await(fut))
              x.base
              assert(Person.getClass.getName == classOf[Person].getName + "$", (Person.getClass.getName, classOf[Person].getName))
              x.name
            }
           }
         }
        """
    assertEquals("bob", run(code))
  }

  @Test
  def testScalaConcurrentAsyncNested(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.async.Async.{async, await}
        |
        |object Test {
        |  def foo[T](a0: Int)(b0: Int*) = s"a0 = $a0, b0 = ${b0.head}"
        |
        |  def test: String = Await.result(async {
        |    var i = 0
        |    def get = async{i += 1; i}
        |    foo[Int](await(get))(await(get) :: await(async(Nil)) : _*)
        |  }, Duration.Inf)
        |}
        |""".stripMargin
    assertEquals("a0 = 1, b0 = 2", run(code))
  }

  @Test
  def testCustomAsync(): Unit = {
    val code = """
       |import scala.tools.nsc.async.{autoawait, customAsync}
       |
       |object Test {
       |  @customAsync
       |  def test: Any = {
       |    val x = reverse("abc")
       |    val y = reverse(x)
       |    (x, y)
       |  }
       |  @autoawait def reverse(a: String) = a.reverse
       |}
       |
       |""".stripMargin
    assertEquals(("cba", "abc"), run(code))
  }

  @Test
  def testMixedAsync(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global, scala.async.Async._
      |
      |object Test {
      |  @customAsync
      |  def test: Any = {
      |    class C {
      |      def repeat(s: String, i: Int): Future[String] = async {
      |        if (i == 0) s
      |        else await(repeat(s, i - 1)) + s
      |      }
      |    }

      |    val x = reverse("abc")
      |    val y = reverse(x)
      |    val z = Await.result(new C().repeat("-", 5), Duration.Inf)
      |    (x, y, z)
      |  }
      |  @autoawait def reverse(a: String) = a.reverse
      |}
      |
      |""".stripMargin
    assertEquals(("cba", "abc", "------"), run(code))
  }


  @Test
  def testExtractor(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |
      |object Test {
      |
      |  object Extractor1 {
      |    @autoawait def unapply(a: String) = Some((a + 1, a + 2))
      |  }
      |  object Extractor2 {
      |    @autoawait def unapply(a: String) = Some(a + 3)
      |  }
      |  @customAsync
      |  def test: Any = {
      |    @autoawait def id(a: String) = a
      |
      |    println("Test.test")
      |    val r1 = Predef.identity("blerg") match {
      |      case x if " ".isEmpty                                   => "case 2: " + x
      |      case Extractor1(Extractor2(x), y: String) if x == "xxx" => "case 1: " + x + ":" + y
      |        x match {
      |          case Extractor1(Extractor2(x), y: String) =>
      |          case _                                    =>
      |        }
      |      case Extractor2(x)                                      => "case 3: " + x
      |    }
      |    r1
      |  }
      |}
      |
      |""".stripMargin
    assertEquals("case 3: blerg3", run(code))
  }


  @Test
  def testLocalModule(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |
      |object Test {
      |  @customAsync def test: Any = {
      |    object Foo {
      |      @autoawait def id(a: String) = a
      |    }
      |    (Foo.id("a"), Foo.id("b"))
      |  }
      |}
      |
      |""".stripMargin
    assertEquals(("a", "b"), run(code))
  }

  @Test
  def testGuard(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |
      |object Test extends App {
      |  @customAsync
      |  def test: Any = {
      |    @autoawait def id[A](a: A) = a
      |
      |    "" match {
      |      case _ if id(false) => ???;
      |      case _              => id("okay")
      |    }
      |  }
      |}
      |
      |""".stripMargin
    assertEquals("okay", run(code))
  }


  @Test
  def testNestedMatchExtractor(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |
      |object Test extends App {
      |  @customAsync def test = {
      |    object Extractor {
      |      @autoawait def unapply(a: String) = Some((a, a))
      |    }
      |    "" match {
      |      case _ if "".isEmpty =>
      |        "" match {
      |          case Extractor(a, b) => a == b
      |        }
      |    }
      |  }
      |}
      |
      |""".stripMargin
    assertEquals(true, run(code))
  }



  // Handy to debug the compiler
  @Test @Ignore
  def testManualRunPartestUnderJUnit(): Unit = {
    val code = new String(Files.readAllBytes(Paths.get("../async/run/concurrent_ArrayIndexOutOfBoundIssue.scala")))
    assertEquals(("a", "b"), run(code))
  }

  private def createTempDir(): File = {
    val f = File.createTempFile("output", "")
    f.delete()
    f.mkdirs()
    f
  }

  def run(code: String): Any = {
    val out = createTempDir()
    try {
      val reporter = new StoreReporter {
        override protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
          if (severity == INFO) println(msg)
          else super.info0(pos, msg, severity, force)
        }
      }
      val settings = new Settings(println(_))
      settings.async.value = true
      settings.outdir.value = out.getAbsolutePath
      settings.embeddedDefaults(getClass.getClassLoader)

      // settings.debug.value = true
      // settings.uniqid.value = true
      // settings.processArgumentString("-Xprint:async -nowarn")
      // settings.log.value = List("async")

      // NOTE: edit ANFTransform.traceAsync to `= true` to get additional diagnostic tracing.

      val isInSBT = !settings.classpath.isSetByUser
      if (isInSBT) settings.usejavacp.value = true
      val global = new Global(settings, reporter) {
        self =>

        object late extends {
          val global: self.type = self
        } with AnnotationDrivenAsyncPlugin

        override protected def loadPlugins(): List[Plugin] = late :: Nil
      }
      import global._

      val run = new Run
      val source = newSourceFile(code)
      run.compileSources(source :: Nil)
      Assert.assertTrue(reporter.infos.mkString("\n"), !reporter.hasWarnings)
      Assert.assertTrue(reporter.infos.mkString("\n"), !reporter.hasErrors)
      val loader = new URLClassLoader(Seq(new File(settings.outdir.value).toURI.toURL), global.getClass.getClassLoader)
      val cls = loader.loadClass("Test")
      cls.getMethod("test").invoke(null) match {
        case t: scala.concurrent.Future[_] =>
          scala.concurrent.Await.result(t, Duration.Inf)
        case cf: CustomFuture[_] =>
          cf._block
        case value => value
      }
    } finally {
      scala.reflect.io.Path.apply(out).deleteRecursively()
    }
  }
}

abstract class AnnotationDrivenAsyncPlugin extends Plugin {

  import global._

  override val components: List[PluginComponent] = List(new PluginComponent with TypingTransformers {
    val global: AnnotationDrivenAsyncPlugin.this.global.type = AnnotationDrivenAsyncPlugin.this.global

    lazy val asyncModuleSym = symbolOf[CustomFuture.type]
    lazy val awaitSym = symbolOf[CustomFuture.type].info.member(TermName("_await"))
    lazy val autoAwaitSym = symbolOf[autoawait]
    lazy val customAsyncSym = symbolOf[customAsync]
    lazy val CustomFuture_class = symbolOf[CustomFuture.type]
    lazy val CustomFuture_successful = CustomFuture_class.companionModule.info.member(TermName("_successful"))

    def newTransformer(unit: CompilationUnit) = new TypingTransformer(unit) {
      override def transform(tree: Tree): Tree = {
        def wrapAwait = {
          localTyper.typedPos(tree.pos) {
            Apply(TypeApply(gen.mkAttributedRef(asyncModuleSym.typeOfThis, awaitSym), TypeTree(tree.tpe) :: Nil), gen.mkMethodCall(CustomFuture_successful, tree :: Nil) :: Nil)
          }
        }
        super.transform(tree) match {
          case ap@Apply(fun, _) if fun.symbol.hasAnnotation(autoAwaitSym) =>
            wrapAwait
          case sel@Select(_, _) if sel.symbol.hasAnnotation(autoAwaitSym) && !(tree.tpe.isInstanceOf[MethodType] || tree.tpe.isInstanceOf[PolyType]) =>
            wrapAwait
          case dd: DefDef if dd.symbol.hasAnnotation(customAsyncSym) =>
            deriveDefDef(dd) {
              rhs =>
                val unit = localTyper.context.unit
                val applyMethod =
                  q"""def apply(tr: _root_.scala.util.Either[_root_.scala.Throwable, _root_.scala.AnyRef]): _root_.scala.Unit = {$rhs; () }"""
                applyMethod.updateAttachment(ChangeOwnerAttachment(dd.symbol))
                global.async.addFutureSystemAttachment(unit, applyMethod, CustomFutureFutureSystem)
                val wrapped =
                  q"""
                    {
                      class stateMachine$$async extends _root_.scala.tools.nsc.async.StateMachineBase {
                       $applyMethod
                      }
                      val stateMachine$$async = new stateMachine$$async
                      _root_.scala.tools.nsc.async.CustomFuture._unit._onComplete(
                        stateMachine$$async.asInstanceOf[_root_.scala.util.Either[_root_.scala.Throwable, _root_.scala.Unit] => _root_.scala.Unit]
                      )
                      stateMachine$$async.result$$async._future
                    }
                   """

                val tree =
                  q"""
                      val temp = ${wrapped}
                     temp._block
                    """
                val result = atOwner(dd.symbol) {
                  localTyper.typedPos(dd.pos) {
                    tree
                  }
                }
                result
            }
          case x => x
        }
      }
    }

    override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        newTransformer(unit).transformUnit(unit)
      }
    }

    override val runsAfter: List[String] = "refchecks" :: "patmat" :: Nil
    override val phaseName: String = "postpatmat"

  })
  override val description: String = "postpatmat"
  override val name: String = "postpatmat"
}

// Calls to methods with this annotation are translated to `Async.await(Future.successful(<call>))`
// This lets us express and test async boundaries in extractor calls, which one can't do with the async/await macro.
final class autoawait extends StaticAnnotation

final class customAsync extends StaticAnnotation

abstract class StateMachineBase extends Function1[scala.util.Either[Throwable, AnyRef], Unit] {
  var result$async: CustomPromise[AnyRef] = new CustomPromise[AnyRef](scala.concurrent.Promise.apply[AnyRef]);
  var state$async: Int = StateAssigner.Initial
  def apply(tr$async: scala.util.Either[Throwable, AnyRef]): Unit

  // Adapter methods
  protected def completeFailure(t: Throwable): Unit = result$async._complete(Left(t))
  protected def completeSuccess(value: AnyRef): Unit = result$async._complete(Right(value))
  protected def onComplete(f: CustomFuture[AnyRef]): Unit = f._onComplete(this)
  protected def getCompleted(f: CustomFuture[AnyRef]): Either[Throwable, AnyRef] = f._getCompleted
  protected def tryGet(tr: Either[Throwable, AnyRef]): AnyRef = tr match {
    case Right(value) =>
      value
    case Left(throwable) =>
      result$async._complete(tr)
      this // sentinel value to indicate the dispatch loop should exit.
  }
}

object CustomFutureFutureSystem extends FutureSystem {
  def Async_await(global: Global): global.Symbol = global.symbolOf[CustomFuture.type].info.member(global.TermName("_await"))
  override def continueCompletedFutureOnSameThread: Boolean = false
}
