package scala.tools.nsc.transform

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.{Global, Phase, Settings, SubComponent}
import scala.tools.nsc.reporters.StoreReporter

/** A phase changes a member's info between refchecks and Erasure's double-definition check.
 *
 *  The check asks whether a pair matched at refchecks, and may read the answer RefChecks recorded rather than compute
 *  it. Either way, `DoubleDefsCursor` excludes a symbol whose history no longer reaches refchecks, as `setInfo` leaves it.
 *
 *  The change runs after patmat because `exitingRefchecks` reads infos at the phase right after refchecks: an
 *  `updateInfo` in that phase would be seen by the computation, though not by the recorded answer.
 */
class ErasureDoubleDefTest {
  private class ChangingGlobal(settings: Settings, reporter: StoreReporter, change: ChangingGlobal => Unit)
      extends Global(settings, reporter) { self =>
    override protected def computeInternalPhases(): Unit = {
      super.computeInternalPhases()
      addToPhasesSet(new SubComponent {
        val global: self.type = self
        val phaseName = "changeinfos"
        val runsAfter = List("patmat")
        val runsRightAfter = None
        override val runsBefore = List("uncurry")
        def newPhase(prev: Phase): Phase = new StdPhase(prev) {
          override def run(): Unit = change(self)
          def apply(unit: CompilationUnit): Unit = ()
        }
      }, "changes member infos after patmat")
    }

    def f(owner: String): Symbol = rootMirror.getRequiredClass(s"p.$owner").info.decl(TermName("f"))

    /** The type of `def f(x: List[Int]): Int`. */
    def takesListOfInt(f: Symbol): Type =
      MethodType(f.newSyntheticValueParams(List(appliedType(definitions.ListClass, definitions.IntTpe))), definitions.IntTpe)
  }

  private def errors(code: String)(change: ChangingGlobal => Unit): List[String] = {
    val settings = new Settings
    settings.usejavacp.value = true
    settings.stopAfter.value = List("posterasure")
    val reporter = new StoreReporter(settings)
    val global = new ChangingGlobal(settings, reporter, change)
    new global.Run().compileSources(List(new BatchSourceFile("test.scala", code)))
    reporter.infos.toList.filter(_.severity == reporter.ERROR).map(_.msg.linesIterator.next())
  }

  // The members of `C` match at refchecks, so RefChecks records the pair.
  private val recorded =
    """package p
      |trait A { def f(x: List[String]): Int }
      |abstract class C extends A { def f(x: List[String]): Int }
      |""".stripMargin

  // The members of `C` don't match, nor do their types erase alike, so RefChecks doesn't record the pair and Erasure doesn't object.
  private val unrecorded =
    """package p
      |trait A { def f(x: List[String]): Int }
      |trait B { def f(x: Set[Int]): Int }
      |abstract class C extends A with B
      |""".stripMargin

  private val clash = "name clash between"

  private def assertClash(messages: List[String]): Unit =
    assertEquals(messages.toString, List(true), messages.map(_.startsWith(clash)))

  @Test def clashesWithoutAnyChange(): Unit =
    assertClash(errors(
      """package p
        |trait A { def f(x: List[String]): Int }
        |trait B { def f(x: List[Int]): Int }
        |abstract class C extends A with B
        |""".stripMargin)(_ => ()))

  @Test def setInfoExcludesUnrecordedPair(): Unit =
    assertEquals(Nil, errors(unrecorded) { g =>
      val f = g.f("B")
      f.setInfo(g.takesListOfInt(f))
    })

  @Test def setInfoExcludesRecordedPair(): Unit =
    assertEquals(Nil, errors(recorded) { g =>
      val f = g.f("A")
      f.setInfo(g.takesListOfInt(f))
    })

  // `updateInfo` keeps the history, so the check still sees what refchecks saw.
  @Test def updateInfoKeepsRecordedPairMatching(): Unit =
    assertEquals(Nil, errors(recorded) { g =>
      val f = g.f("A")
      f.updateInfo(g.takesListOfInt(f))
    })

  @Test def updateInfoLeavesUnrecordedPairUnmatched(): Unit =
    assertClash(errors(unrecorded) { g =>
      val f = g.f("B")
      f.updateInfo(g.takesListOfInt(f))
    })
}
