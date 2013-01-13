import scala.tools.partest._

object Test extends IcodeTest {
  override def printIcodeAfterPhase = "inline"
}

object A {
  @inline def f1(name: String) = Class.forName(name, true, this.getClass.getClassLoader)
  @inline def f2(name: String) = Class.forName(name)
}

class B {
  def g1(name: String) = A f1 "Bippy"
  def g2(name: String) = A f2 "Bippy"
}
