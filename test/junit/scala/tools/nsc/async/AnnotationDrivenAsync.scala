package scala.tools.nsc
package async

import java.io.File
import java.lang.reflect.InvocationTargetException
import java.nio.file.{Files, Paths}
import java.util.concurrent.CompletableFuture

import org.junit.Assert.assertEquals
import org.junit.{Assert, Ignore, Test}

import scala.annotation.StaticAnnotation
import scala.concurrent.duration.Duration
import scala.reflect.internal.util.Position
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.partest.async.AsyncStateMachine

class AnnotationDrivenAsync {
  @Test
  @Ignore // TODO XASYNC
  def testBoxedUnitNotImplemented(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |import Future.successful
        |class A {
        |  def f = successful(this)
        |}
        |object Test {
        |  val data = List(("0", "0"))
        |  def test = async {
        |    val s1 = await(new A().f)
        |    s1.toString
        |    val s2 = await(s1.f)
        |    s2.toString
        |    val it = data.iterator
        |    while (it.hasNext) {
        |      val v = it.next()
        |      v match {
        |        case (x, y) =>
        |          "".isEmpty
        |          val r1 = await(s1.f).toString
        |          val r2 = await(s1.f).toString
        |          (r1, r2)
        |          val it = Nil.iterator
        |          while (it.hasNext) {
        |            val v = it.next()
        |            val r = await(s1.f).equals(v)
        |          }
        |      }
        |    }
        |  }
        |}
        |""".stripMargin
     assertEquals((), run(code))
  }

  @Test
  def testBasicScalaConcurrentCapture(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
        |
        |object Test {
        |  def test: Future[(String, Int, Int)] = async { var x = "init"; val y = await(f(1)); class C { x = x + "_updated" }; new C; (x, y, await(f(2))) }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(("init_updated", 1, 2), run(code))
  }

  @Test
  def testScalaConcurrentAsyncNested(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.partest.async.Async.{async, await}
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
      |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global, scala.tools.partest.async.Async._
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

  @Test def testByNameOwner(): Unit = {
    val result = run(
      """
      import scala.tools.nsc.async.{autoawait, customAsync}

      object Bleh {
        @autoawait def asyncCall(): Int = 0
        def byName[T](fn: => T): T = fn
      }
      object Boffo {
        @autoawait @customAsync def jerk(): Unit = {
          val pointlessSymbolOwner = 1 match {
            case _ =>
              Bleh.asyncCall()
              Bleh.byName {
                val whyDoHateMe = 1
                whyDoHateMe
              }
          }
        }
      }
      object Test {
        @customAsync def test() = Boffo.jerk()
      }
      """)
  }

  @Test def testByNameOwner2(): Unit = {
    val result = run(
      """
      import scala.tools.nsc.async.{autoawait, customAsync}
      object Bleh {
        @autoawait def bleh = Bleh
        def byName[T](fn: => T): T = fn
      }
      object Boffo {
        @autoawait @customAsync def slob(): Unit = {
          val pointlessSymbolOwner =  {
              Bleh.bleh.byName {
                val whyDoHateMeToo = 1
                whyDoHateMeToo
              }
          }
        }
      }
      object Test {
        @customAsync def test() = Boffo.slob()
      }
      """)
  }

  @Test def testRewrittenApply(): Unit = {
    val result = run(
      """
        |import scala.tools.nsc.async.{autoawait, customAsync}
        |object O {
        |  case class Foo(a: Any)
        |}
        |object Test {
        |  @autoawait def id(a: String) = a
        |  @customAsync
        |  def test = {
        |    O.Foo
        |    id("foo") + id("bar")
        |    O.Foo(1)
        |  }
        |}
        | """.stripMargin)
    assertEquals("Foo(1)", result.toString)
  }

  @Test def testIsInstanceOfType(): Unit = {
    val result = run(
      """ import scala.tools.nsc.async.{autoawait, customAsync}
        |
        | class Outer
        | object Test {
        |   @autoawait def id(a: String) = a
        |   @customAsync def test = {
        |     val o = new Outer
        |     id("foo") + id("bar")
        |     ("": Object).isInstanceOf[o.type]
        |   }
        | }
        | """.stripMargin)
    assertEquals(false, result)
  }

  @Test def testIsInstanceOfTerm(): Unit = {
    val result = run(
      """import scala.tools.nsc.async.{autoawait, customAsync}
        |
        | class Outer
        | object Test {
        |   @autoawait def id(a: String) = a
        |   @customAsync def test = {
        |     val o = new Outer
        |     id("foo") + id("bar")
        |     o.isInstanceOf[Outer]
        |   }
        | }
        | """.stripMargin)
    assertEquals(true, result)
  }

  @Test def testArrayLocalModule(): Unit = {
    val result = run(
      """ import scala.tools.nsc.async.{autoawait, customAsync}
        |
        | class Outer
        | object Test {
        |   @autoawait def id(a: String) = a
        |   @customAsync def test = {
        |     val O = ""
        |     id("foo") + id("bar")
        |     new Array[O.type](0)
        |   }
        | }
        | """.stripMargin)
    assertEquals(classOf[Array[String]], result.getClass)
  }

  @Test def lambdafiedCustomAsync(): Unit = {
    val result = run(
      """ import scala.tools.nsc.async.{autoawait, customAsync}
        |
        | object Util {
        |    @autoawait def id(a: String) = a
        |    def reflect = {
        |      val classes = new java.io.File(Test.getClass.getProtectionDomain.getCodeSource.getLocation.toURI).listFiles.toList.map(_.getName).sorted.mkString(",")
        |      val methods = List(Test.getClass, classOf[C]).flatMap(_.getDeclaredMethods).filter(_.getName.contains("fsm")).sortBy(_.getName).mkString("\n")
        |      classes + "\n" + methods
        |   }
        | }
        | class Outer
        | class C {
        |   import Util.id
        |   def foo = 42
        |   @customAsync def test = {
        |     id("a")
        |     id("b")
        |     foo
        |     class Inner1 {
        |        def needOuter = C.this
        |     }
        |     assert(new Inner1().needOuter eq this)
        |     Util.reflect
        |   }
        |
        |  @customAsync def testStatic = {
        |     id("a")
        |     id("b")
        |     id("c")
        |   }
        | }
        | object Test extends C {
        |   import Util._
        |   @customAsync def staticInModule = {
        |     id("a")
        |     id("b")
        |     id("c")
        |   }
        | }
        | """.stripMargin)
    val expected =
      """|C$Inner1$1.class,C.class,Outer.class,Test$.class,Test.class,Util$.class,Util.class
         |public static final java.lang.Object C.fsm$1(C,scala.tools.nsc.async.GenericCustomFutureStateMachine,scala.util.Either)
         |public static final java.lang.Object C.fsm$2(scala.tools.nsc.async.GenericCustomFutureStateMachine,scala.util.Either)
         |public static final java.lang.Object Test$.fsm$3(scala.tools.nsc.async.GenericCustomFutureStateMachine,scala.util.Either)""".stripMargin.trim
    assertEquals(expected, result)
  }

  // Handy to debug the compiler or to collect code coverage statistics in IntelliJ.
  @Test
  @Ignore
  def testManualRunPartestUnderJUnit(): Unit = {
    import scala.collection.JavaConverters._
    for (path <- List(Paths.get("../async/run"), Paths.get("../async/neg"))) {
      for (file <- Files.list(path).iterator.asScala) {
        if (file.getFileName.toString.endsWith(".scala")) {
          val code = new String(Files.readAllBytes(file))
          run(code, compileOnly = true)
        }
      }
    }
  }

  private def createTempDir(): File = {
    val f = File.createTempFile("output", "")
    f.delete()
    f.mkdirs()
    f
  }

  def run(code: String, compileOnly: Boolean = false): Any = {
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
      // settings.processArgumentString("-Xprint:typer,erasure,async,postasync -nowarn")
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
      if (compileOnly) return null
      def showInfo(info: StoreReporter#Info): String = {
        Position.formatMessage(info.pos, info.severity.toString.toLowerCase + " : " + info.msg, false)
      }
      Assert.assertTrue(reporter.infos.map(showInfo).mkString("\n"), !reporter.hasErrors)
      Assert.assertTrue(reporter.infos.map(showInfo).mkString("\n"), !reporter.hasWarnings)
      val loader = new URLClassLoader(Seq(new File(settings.outdir.value).toURI.toURL), global.getClass.getClassLoader)
      val cls = loader.loadClass("Test")
      val result = try {
        cls.getMethod("test").invoke(null)
      } catch {
        case ite: InvocationTargetException => throw ite.getCause
        case _: NoSuchMethodException =>
          cls.getMethod("main", classOf[Array[String]]).invoke(null, null)
      }
      result match {
        case t: scala.concurrent.Future[_] =>
          scala.concurrent.Await.result(t, Duration.Inf)
        case cf: CustomFuture[_] =>
          cf._block
        case cf: CompletableFuture[_] =>
          cf.get()
        case value => value
      }
    } catch {
      case ve: VerifyError =>
        val asm = out.listFiles().filter(_.getName.contains("stateMachine")).flatMap { file =>
          import scala.sys.process._
          val javap = List("/usr/local/bin/javap", "-v", file.getAbsolutePath).!!
          val asmp = AsmUtils.textify(AsmUtils.readClass(file.getAbsolutePath))
          javap :: asmp :: Nil
        }.mkString("\n\n")
        throw new AssertionError(asm, ve)
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
                val fsmMethod =
                  q"""def fsm(tr: _root_.scala.util.Either[${definitions.ThrowableTpe}, ${definitions.AnyRefTpe}]): ${definitions.UnitTpe} = $rhs"""
                val fsmMethodMarked = global.async.markForAsyncTransform(dd.symbol, fsmMethod, awaitSym, Map.empty)
                val name = TypeName("stateMachine$async")
                val wrapped =
                  q"""
                    class $name extends _root_.scala.tools.nsc.async.CustomFutureStateMachine {
                     $fsmMethodMarked
                    }
                    new $name().start()
                   """

                val tree =
                  q"""
                     val temp = ${wrapped.updateAttachment(FsmBlock)}
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
  }, new PluginComponent with TypingTransformers {
    val global: AnnotationDrivenAsyncPlugin.this.global.type = AnnotationDrivenAsyncPlugin.this.global
    lazy val GenericCustomFutureStateMachine = rootMirror.getClassIfDefined("scala.tools.nsc.async.GenericCustomFutureStateMachine")
    lazy val SetFsmAndStartSymbol = GenericCustomFutureStateMachine.info.decl(TermName("setFsmAndStart"))

    def newTransformer(unit: CompilationUnit) = new TypingTransformer(unit) { tt =>
      override def transform(tree: Tree): Tree = tree match {
        case Block((cd: ClassDef) :: Nil, Apply(Select(Apply(fsmInit, params), _), Nil)) if tree.hasAttachment[FsmBlock.type] =>
          // { class stateMachine { def fsm(...): Unit = { ... } }
          // val fsm = new GenericCustomeFutureStateMachine
          // fsm.SetFsmAndStartSymbol {
          //   def fsm(...): Object = { .... ; BoxedUnit }
          //   (tr => fsm(tr)) // LambdaMetaFactory will spin up a lambda class with fsm as the impl method.
          // }
          val fields = cd.symbol.info.decls.filter(_.isField).toList

          if (fields.isEmpty || (fields.length == 1 && params.length == 1)) {
            val self       = currentOwner.newTermSymbol(currentUnit.freshTermName("stateMachine"), tree.pos, newFlags = Flag.ARTIFACT).setInfo(GenericCustomFutureStateMachine.tpeHK)
            val selfValDef = localTyper.typedPos(tree.pos)(ValDef(self, New(GenericCustomFutureStateMachine)))

            async.lambdafy(tt.currentOwner,
                           tt.localTyper,
                           cd,
                           params,
                           selfValDef,
                           (selfSym, lambdaExpr) => localTyper.typedPos(tree.pos)(
                             gen.mkMethodCall(gen.mkAttributedIdent(selfSym), SetFsmAndStartSymbol, Nil, lambdaExpr :: Nil))
                           )
          } else
            super.transform(tree)
        case _ =>
          super.transform(tree)
      }
    }

    override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        newTransformer(unit).transformUnit(unit)
      }
    }

    override val runsAfter: List[String] = "async" :: Nil
    override val runsBefore: List[String] = "lambdalift" :: Nil
    override val phaseName: String = "postasync"
  })
  private case object FsmBlock extends PlainAttachment
  override val description: String = "custom-async"
  override val name: String = "customasync"
}

// Calls to methods with this annotation are translated to `Async.await(Future.successful(<call>))`
// This lets us express and test async boundaries in extractor calls, which one can't do with the async/await macro.
final class autoawait extends StaticAnnotation

final class customAsync extends StaticAnnotation

abstract class CustomFutureStateMachine extends AsyncStateMachine[CustomFuture[AnyRef], scala.util.Either[Throwable, AnyRef]] with Function1[scala.util.Either[Throwable, AnyRef], Unit] {
  private val result$async: CustomPromise[AnyRef] = new CustomPromise[AnyRef](scala.concurrent.Promise.apply[AnyRef]);
  private[this] var state$async: Int = 0
  protected def state: Int = state$async
  protected def state_=(s: Int): Unit = state$async = s
  def fsm(tr$async: R[AnyRef]): Unit
  final def apply(tr$async: R[AnyRef]): Unit = fsm(tr$async)

  type F[A] = CustomFuture[A]
  type R[A] = Either[Throwable, A]
  // Adapter methods
  protected def completeFailure(t: Throwable): Unit = result$async._complete(Left(t))
  protected def completeSuccess(value: AnyRef): Unit = result$async._complete(Right(value))
  protected def onComplete(f: F[AnyRef]): Unit = f._onComplete(this)
  protected def getCompleted(f: F[AnyRef]): R[AnyRef] = f._getCompleted
  protected def tryGet(tr: R[AnyRef]): AnyRef = tr match {
    case Right(value) =>
      value
    case Left(throwable) =>
      result$async._complete(tr)
      this // sentinel value to indicate the dispatch loop should exit.
  }
  def start(): CustomFuture[AnyRef] = {
    CustomFuture._unit.asInstanceOf[CustomFuture[AnyRef]]._onComplete(this)
    result$async._future
  }
}
final class GenericCustomFutureStateMachine extends CustomFutureStateMachine {
  private[this] var f: Either[Throwable, AnyRef] => Unit = null
  def setFsmAndStart(f: Either[Throwable, AnyRef] => Unit): CustomFuture[AnyRef] = {
    this.f = f
    start()
  }
  override def fsm(tr$async: Either[Throwable, AnyRef]): Unit = {f(tr$async); this}
}
