import annotation._
import PrintAttachments._

object Test {
  def namesDefaults1(a: Int = 0, b: Int = 0) = 0
  def namesDefaults2(@deprecatedName('aa) a: Int = 0, b: Int = 0) = 0
  def namesDefaults3(a: Int)(b: Int = 0) = 0
  def namesDefaults4[T](a: T = ???, b: T = ???) = 0
  class C[X](a: Any = 0)(b: Any = 0, c: Any = 0)

  printAttachments(namesDefaults1(0, 1))
  printAttachments(namesDefaults1())
  printAttachments(namesDefaults1(b = 0))
  printAttachments(namesDefaults1(0, 1))
  printAttachments(namesDefaults2(aa = 0, 1))
  printAttachments(namesDefaults3(0)(b = 0))
  printAttachments(namesDefaults3(0)(b = 0))
  printAttachments(namesDefaults4[String](b = ""))
  printAttachments(new C(a = 1)(c = 0))
}
