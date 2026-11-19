package scala.tools.nsc.parser

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._

import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testkit.BytecodeTesting

@RunWith(classOf[JUnit4])
class ParserTest extends BytecodeTesting {
  override def compilerArgs: String = "-Ystop-after:parser -Yvalidate-pos:parser -Yrangepos"
  @Test
  def crlfRangePositionXml_t10321(): Unit = {
    val code =
      """
        |object Test {
        |  Nil.map { _ =>
        |    <X />
        |    <Y />
        |  }
        |}
      """.stripMargin
    val crlfCode = code.linesIterator.map(_ + "\r\n").mkString
    val lfCode = code.linesIterator.map(_ + "\n").mkString
    assertNotEquals(crlfCode, lfCode)
    import compiler._, global._
    val run = new Run
    run.compileSources(newSourceFile(lfCode) :: Nil)
    assertFalse(reporter.hasErrors)
    run.compileSources(newSourceFile(crlfCode) :: Nil)
  }

  @Test
  def rangePosOfDefaultInitializer_t12213(): Unit = {
    val code =
      """object Other { var x: Int = _; var y: Int = 42 }"""
    import compiler._, global._
    val run = new Run
    run.compileSources(newSourceFile(code) :: Nil)
    assertFalse(reporter.hasErrors)
    val unit = run.units.toList.head
    def codeOf(pos: Position) = new String(pos.source.content.slice(pos.start, pos.end))
    val List(x, y) = unit.body.collect { case vd : ValDef => vd }.takeRight(2)
    assertEquals("var y: Int = 42", codeOf(y.pos))
    assertEquals("var x: Int = _", codeOf(x.pos))
  }

  @Test
  def `t11572 range pos of use case`: Unit = {
    val code =
      """|/** This is a lengthy comment.
         | *  @usecase f()
         | *  @inheritDoc
         | */""".stripMargin
    import compiler._, global._
    val run = new Run     // DocComment requires run.reporting to deprecate @usecase
    val src = newSourceFile(code)
    val unit = new CompilationUnit(src)
    val codePos = rangePos(src, 0, 0, code.length)
    val doc = DocComment(code, pos = codePos)
    for (docCommentsSupport.UseCase(DocComment(_,p2,_),_,p1) <- doc.useCases)
      List(p1, p2).foreach(p => assertTrue(s"$codePos must include $p", codePos.includes(p)))
    assertTrue(run.reporting.allConditionalWarnings.isEmpty)
    run.reporting.reportSuspendedMessages(unit)
    assertFalse(run.reporting.allConditionalWarnings.isEmpty)
  }

  /** Parses `code` in a thread with a small stack of a fixed size, and returns what was reported. */
  private def parseWithSmallStack(code: String): List[StoreReporter.Info] = {
    import compiler._, global._
    // let the first run initialize the compiler on the test's own stack
    newRun().compileSources(newSourceFile("class Init") :: Nil)
    assertFalse(reporter.hasErrors)

    var result: Either[Throwable, List[StoreReporter.Info]] = null
    val body: Runnable = () => result =
      try {
        newRun().compileSources(newSourceFile(code) :: Nil)
        Right(reporter.asInstanceOf[StoreReporter].infos.toList)
      } catch {
        case t: Throwable => Left(t)
      }
    val thread = new Thread(null, body, "parser-small-stack", 512 * 1024L)
    thread.start()
    thread.join()
    reporter.reset() // the compiler is shared with tests that start a `new Run` without resetting it
    result.fold(t => throw new AssertionError(s"parser crashed with $t", t), identity)
  }

  @Test
  def deepNestingIsReportedAsAnError(): Unit = {
    // far deeper than the stack allows
    val depth = 20000
    def check(prefix: String, open: String, mid: String, close: String, suffix: String): Unit = {
      val infos = parseWithSmallStack(prefix + open * depth + mid + close * depth + suffix)
      assertEquals(infos.mkString("\n"), 1, infos.size)
      val info = infos.head
      assertTrue(info.msg, info.msg.startsWith("nesting too deep: the parser ran out of stack here."))
      assertTrue(info.pos.toString, info.pos.isDefined)
      val nestingStart = prefix.length
      val nestingEnd = nestingStart + open.length * depth
      assertTrue(s"${info.pos.point} not in $nestingStart to $nestingEnd", info.pos.point > nestingStart && info.pos.point <= nestingEnd)
    }
    check("object A { val x = ", "(", "1", ")", " }")
    check("object A { val x = ", "{", "1", "}", " }")
    check("object A { val x: ", "List[", "Int", "]", " = null }")
    check("object A { def x(a: Int) = ", "if (a == 0) 1 else ", "2", "", " }")
    check("object A { val x = ", "a => ", "1", "", " }")
  }

  @Test
  def manyPatternAlternatives(): Unit = {
    val alternatives = List.tabulate(100000)(_.toString).mkString(" | ")
    val infos = parseWithSmallStack(s"object A { def f(x: Int) = x match { case $alternatives => 1 } }")
    assertEquals(infos.mkString("\n"), Nil, infos)
  }
}
