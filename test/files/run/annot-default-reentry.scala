import scala.reflect.internal.Mode
import scala.tools.partest._

// If typing the default of an annotation parameter is abandoned by an exception (as the
// presentation compiler does when it is interrupted), computing the `@defaultArg` annotation
// again must type the default again, not report a recursive default argument.
object Test extends DirectTest {
  def code = """
    package p
    object Use { def m = 0 }
    class a(x: Int = 42) extends scala.annotation.StaticAnnotation
  """

  class Abandon extends RuntimeException("abandoned")

  def show(): Unit = {
    val global = newCompiler()
    import global._
    import analyzer._

    var armed = false
    var done  = false
    object plugin extends AnalyzerPlugin {
      override def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Mode): Type = {
        tree match {
          case Literal(Constant(42)) if armed => armed = false; throw new Abandon
          case _                              =>
        }
        pt
      }
      override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
        tree match {
          case dd: DefDef if dd.name.toString == "m" && !done =>
            done = true
            val param = rootMirror.getRequiredClass("p.a").primaryConstructor.paramss.head.head
            armed = true
            try defaultArgAnnotation(param)
            catch { case e: Abandon => println(s"first: ${e.getMessage}") }
            try println(s"second: ${defaultArgAnnotation(param).map(_.args)}")
            catch { case e: TypeError => println(s"second: ${e.getMessage}") }
          case _ =>
        }
        tpe
      }
    }
    addAnalyzerPlugin(plugin)
    compileString(global)(code)
  }
}
