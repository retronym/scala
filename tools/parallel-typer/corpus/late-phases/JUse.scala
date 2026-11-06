package lp

import java.util.{ArrayList, List => JList}
class ScalaJ extends JBase[String] {
  def get = "x"
  def items(): JList[String] = { val l = new ArrayList[String]; l.add(get); l }
  override def describe(t: String) = super.describe(t) + count
  def p = prot("a", 1, 2)
  def deep = { val n = new JBase.Nested; new n.Deep().m }
  def c(c: JBase.Colour) = c match { case JBase.Colour.RED => 1; case JBase.Colour.GREEN | JBase.Colour.BLUE => 2 }
  def r = raw().size + JBase.LIMIT + JIface.twice(2)
}
trait ScalaI extends JIface[Int] { def extra = describe(1) }
class ScalaII extends ScalaI { def get = 2 }
