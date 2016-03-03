package scala.reflect.macros
package contexts

import java.io.Closeable

import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox

trait Evals {
  self: Context =>

  private class Eval extends Closeable {
    val loader = universe.analyzer.defaultMacroClassloader()
    val evalMirror = ru.runtimeMirror(loader)
    val evalToolBox = evalMirror.mkToolBox()
    val evalImporter = ru.internal.createImporter(universe).asInstanceOf[ru.Importer { val from: universe.type }]
    override def close(): Unit = loader.close()
  }
  private val evalCache = global.perRunCaches.newGeneric(new Eval)

  def eval[T](expr: Expr[T]): T = {
    expr.tree match {
      case global.Literal(global.Constant(value)) =>
        value.asInstanceOf[T]
      case _ =>
        val eval = evalCache()
        val imported = eval.evalImporter.importTree(expr.tree)
        eval.evalToolBox.eval(imported).asInstanceOf[T]
    }
  }
}