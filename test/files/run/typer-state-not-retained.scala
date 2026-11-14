import scala.tools.partest._

// Attachments that hold a Typer or Namer (and through them a context chain and the
// trees of enclosing definitions) are dropped once their last consumer has run.
object Test extends DirectTest {
  def code = """
    case class C(a: Int, b: String = "b")
    object Use {
      def f(x: Int, y: Int = 2) = x + y
      def g = f(y = 1, x = 2) + f(1)
      def h = C(1).copy(b = "c")
    }
  """

  def show(): Unit = {
    def check(stopAfter: String): (Int, Boolean, Boolean) = {
      val global = newCompiler("-nowarn", "-Ystop-after:" + stopAfter)
      import global._
      withRun(global) { run =>
        run.compileSources(newSources(code))
        assert(!global.reporter.hasErrors, "compilation failed")
        var namedApplyInfos = 0
        var module: Symbol = NoSymbol
        object traverser extends Traverser {
          override def traverse(t: Tree): Unit = {
            if (analyzer.NamedApplyBlock.namedApplyInfo(t).isDefined) namedApplyInfos += 1
            t match {
              case md: ModuleDef if md.name.toString == "C" => module = md.symbol
              case tt: TypeTree if tt.original != null => traverse(tt.original)
              case _ =>
            }
            super.traverse(t)
          }
        }
        run.units.foreach(u => traverser.traverse(u.body))
        assert(module.exists, "companion of C not found")
        (namedApplyInfos,
          module.hasAttachment[analyzer.ConstructorDefaultsAttachment],
          module.moduleClass.hasAttachment[analyzer.ClassForCaseCompanionAttachment])
      }
    }
    val (typerInfos, _, _) = check("typer")
    assert(typerInfos > 0, "expected NamedApplyInfo after typer")
    assert(check("refchecks") == ((0, false, false)), check("refchecks"))
  }
}
