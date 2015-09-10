import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object Test {
  def main(args: Array[String]): Unit = {
    val tb = ToolBox(currentMirror).mkToolBox()
    val source = """{
      import scala.language.higherKinds

      class Outer[X] {
        class Inner[Y]
      }
      class Other[T[_]]

      new Other[Outer[Int]#Inner]()
    }"""

    val tree = tb.parse(source)
    tb.typecheck(tree).tpe
  }
}
