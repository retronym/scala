// `lubList` assumes that base type sequences are sorted by `Symbol#isLess`. `NameType` precedes `Name`
// in its base type sequence, but was ranked by its bound as declared in `Api`, which has a shorter
// sequence than the class `Name`. That put `Name` first, so the lub lost it.
trait Api {
  type Name >: Null <: AnyRef
  trait Sym { type NameType >: Null <: Name; def name: NameType }
}
trait Impl extends Api {
  trait A; trait B; trait C; trait D
  abstract class Name extends A with B with C with D
  class TermName extends Name
  def t: TermName
  def s: Sym
  def f(b: Boolean) = { val m = if (b) t else s.name; val n: Name = m; n }
}

// When the ranks tie, the result depended on which symbol was created first. It worked with the
// traits in the other order.
trait Impl2 extends Api2 {
  trait X; trait Y
  abstract class Name extends NameApi with X with Y
  class TermName extends Name
  def t: TermName
  def s: Sym
  def f(b: Boolean) = { val m = if (b) t else s.name; val n: Name = m; n }
}
trait Api2 {
  type Name >: Null <: AnyRef with NameApi
  trait NameApi
  trait Sym { type NameType >: Null <: Name; def name: NameType }
}
