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
import scala.util.Success

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
      // settings.processArgumentString("-Xprint:all -nowarn")
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
                      val ${nme.execContextTemp} = ()
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
  def execContext$async = ()
  var result$async: CustomPromise[AnyRef] = new CustomPromise[AnyRef](scala.concurrent.Promise.apply[AnyRef]);
  var state$async: Int = StateAssigner.Initial
  def apply(tr$async: scala.util.Either[Throwable, AnyRef]): Unit
}

object CustomFutureFutureSystem extends FutureSystem {
  override type Prom[A] = CustomFuture[A]
  override type Fut[A] = CustomPromise[A]
  override type ExecContext = Unit
  override type Tryy[A] = Either[Throwable, A]
  override def mkOps(u: SymbolTable): Ops[u.type] = new Ops[u.type](u) {
    import u._

    private val global = u.asInstanceOf[Global]
    lazy val Future_class: Symbol = rootMirror.requiredClass[CustomFuture[_]]
    lazy val Promise_class: Symbol = rootMirror.requiredClass[CustomPromise[_]]
    lazy val Either_class: Symbol = rootMirror.requiredClass[scala.util.Either[_, _]]
    lazy val Right_class: Symbol = rootMirror.requiredClass[scala.util.Right[_, _]]
    lazy val Left_class: Symbol = rootMirror.requiredClass[scala.util.Left[_, _]]
    lazy val Future_onComplete: Symbol = Future_class.info.member(TermName("_onComplete")).ensuring(_.exists)
    lazy val Future_getCompleted: Symbol = Future_class.info.member(TermName("_getCompleted")).ensuring(_.exists)
    lazy val Future_unit: Symbol = Future_class.companionModule.info.member(TermName("_unit")).ensuring(_.exists)
    lazy val Promise_complete: Symbol = Promise_class.info.member(TermName("_complete")).ensuring(_.exists)
    lazy val Either_isFailure: Symbol = Either_class.info.member(TermName("isLeft")).ensuring(_.exists)
    lazy val Right_get: Symbol = Right_class.info.member(TermName("value")).ensuring(_.exists)

    lazy val Async_async: Symbol = NoSymbol.newTermSymbol(nme.EMPTY)
    lazy val Async_await: Symbol = symbolOf[CustomFuture.type].info.member(TermName("_await"))

    def tryType(tp: Type): Type = appliedType(Either_class, tp)

    def future(a: Tree, execContext: Tree): Tree =
      Apply(Select(gen.mkAttributedStableRef(Future_class.companionModule), TermName("_apply")), List(a))

    def futureUnit(execContext: Tree): Tree =
      mkAttributedSelectApplyIfNeeded(gen.mkAttributedStableRef(Future_class.companionModule), Future_unit)

    def onComplete[A, B](future: Expr[Fut[A]], fun: Expr[scala.util.Try[A] => B],
                         execContext: Expr[ExecContext]): Expr[Unit] = {
      Apply(Select(future, Future_onComplete), fun :: Nil)
    }

    override def continueCompletedFutureOnSameThread: Boolean = true

    def mkAttributedSelectApplyIfNeeded(qual: Tree, sym: Symbol) = {
      val sel = gen.mkAttributedSelect(qual, sym)
      if (isPastErasure) Apply(sel, Nil) else sel
    }

    override def getCompleted[A](future: Expr[Fut[A]]): Expr[Tryy[A]] = {
      mkAttributedSelectApplyIfNeeded(future, Future_getCompleted)
    }

    def completeProm[A](prom: Expr[Prom[A]], value: Expr[scala.util.Try[A]]): Expr[Unit] = {
      gen.mkMethodCall(prom, Promise_complete, Nil, value :: Nil)
    }

    def tryyIsFailure[A](tryy: Expr[scala.util.Try[A]]): Expr[Boolean] = {
      mkAttributedSelectApplyIfNeeded(tryy, Either_isFailure)
    }

    def tryyGet[A](tryy: Expr[Tryy[A]]): Expr[A] = {
      mkAttributedSelectApplyIfNeeded(gen.mkCast(tryy, Right_class.tpe_*), Right_get)
    }

    def tryySuccess[A](a: Expr[A]): Expr[Tryy[A]] = {
      assert(isPastErasure)
      New(Right_class, a)
    }

    def tryyFailure[A](a: Expr[Throwable]): Expr[Tryy[A]] = {
      assert(isPastErasure)
      New(Left_class, a)
    }
  }
}
