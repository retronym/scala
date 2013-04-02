import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.tools.reflect.Eval

object Test extends App {
  object Extractor { def unapply(x: Int): Option[Int] = Some(x) }
  val extractor = reify {
    2 match { case Extractor(x) => x }
  }
  println(extractor.eval)
  val tb = cm.mkToolBox()
  val textractor = tb.typeCheck(extractor.tree)
  val rtextractor = tb.resetAllAttrs(textractor)

  def showTree(label: String, tree: Tree) = {
    println("=" * 60)
    println(label)
    println("=" * 60)
    println()
    println(show(tree))
    println(showRaw(tree))
  }
  showTree("untyped", extractor.tree)
  showTree("typed", textractor)
  showTree("reset", rtextractor)
  try {
    println(tb.eval(rtextractor))
  } catch {
    // this is the current behaviour, rather than the desired behavior; see SI-5465
    case ex: ToolBoxError => println(ex.getMessage)
  }
}