package scala.tools.nsc
package interactive
package tests.core

import reporters.{Reporter => CompilerReporter}
import scala.tools.nsc.interactive.InteractiveReporter
import scala.reflect.internal.util.SourceFile

class MultiRunTest { self =>
  private val settings = new Settings
  settings.usejavacp.value = true

  private object Break extends scala.util.control.ControlThrowable

  private val compilerReporter: CompilerReporter = new InteractiveReporter {
    override def compiler = self.compiler
  }

  object compiler extends Global(settings, compilerReporter) {
    override def checkForMoreWork(pos: Position) {
    }
    override def signalDone(context: Context, old: Tree, result: Tree) {
      if (!interrupted && analyzer.lockedCount == 0 && interruptsEnabled && shouldInterrupt(result)) {
        interrupted = true
        afterInterrupt()
      }
      super.signalDone(context, old, result)
    }

    // we're driving manually using our own thread, disable the check here.
    override def assertCorrectThread() {}
  }

  import compiler._

  private var interrupted = false

  def code: String = """
      package p1
      class MagicInterruptionMarker // stop when we typecheck this
      object Foo {
        trait T
        case class Bar() extends T
        def foo(b: T) = b
      }
    """
  def shouldInterrupt(tree: Tree): Boolean = {
    tree.symbol != null && tree.symbol.name.toString == "MagicInterruptionMarker"
  }
  def afterInterrupt() {
    val sym = rootMirror.getModuleIfDefined("p1.Foo").moduleClass
    assert(!sym.hasCompleteInfo)
    assert(currentRun.compiles(sym))
    new TyperRun()
    assert(!currentRun.compiles(sym))
    sym.initialize
  }

  def checkTypedTree(tree: Tree): Unit = {}

  private val source: SourceFile = newSourceFile(code)

  def assertNoProblems() {
    val problems = getUnit(source).get.problems
    assert(problems.isEmpty, problems.mkString("\n"))
  }

  def show() {
    reloadSource(source)
    typedTree(source, true)
    assertNoProblems()
  }

  def main(args: Array[String]) { show() }
}
