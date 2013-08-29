import scala.tools.partest._

object Test extends ReplTest {
  override def code = """
:power
object O { final val x = 22; final val y = "" }
val t1 = reify { O.x }
val t2 = reify { 1 + 1 }"""
}