// The null test on the sequence selected by a sequence pattern can't fail for a value class's
// unboxed representation, but it can for a box at a generic position, or for a reference.
import collection.SeqFactory.UnapplySeqWrapper

class V(val xs: List[String]) extends AnyVal {
  def isEmpty = false
  def get = this
  def lengthCompare(n: Int) = xs.lengthCompare(n)
  def apply(i: Int) = xs(i)
  def drop(n: Int) = xs.drop(n)
  def toSeq = xs
}
object Direct      { def unapplySeq(a: Int) = new V(List("x")) }
object NullInV     { def unapplySeq(a: Int) = new V(null) }
object SomeNull    { def unapplySeq(a: Int): Option[List[String]] = Some(null) }
object OptV        { def unapplySeq(a: Int): Option[V] = Some(new V(List("x"))) }
object OptNullV    { def unapplySeq(a: Int): Option[V] = Some(null).asInstanceOf[Option[V]] }
object TupV        { def unapplySeq(a: Int): Option[(Int, V)] = Some((1, new V(List("x")))) }
object TupNullV    { def unapplySeq(a: Int): Option[(Int, V)] = Some((1, null)).asInstanceOf[Option[(Int, V)]] }
object Wrapped     { def unapplySeq(a: Int) = new UnapplySeqWrapper[String](List("x")) }
object NullWrapped { def unapplySeq(a: Int) = new UnapplySeqWrapper[String](null) }
object OptWrapper  { def unapplySeq(a: Int): Option[UnapplySeqWrapper[String]] = Some(new UnapplySeqWrapper[String](List("x"))) }
object OptNullWrap { def unapplySeq(a: Int): Option[UnapplySeqWrapper[String]] = Some(null).asInstanceOf[Option[UnapplySeqWrapper[String]]] }

object Test {
  def t(name: String)(f: => String) =
    println(s"$name: ${try f catch { case e: NullPointerException => "NPE" }}")

  def main(args: Array[String]): Unit = {
    t("Direct")(1 match { case Direct(a) => a case _ => "no match" })
    t("NullInV")(1 match { case NullInV(a) => a case _ => "no match" })
    t("SomeNull")(1 match { case SomeNull(a) => a case _ => "no match" })
    t("OptV")(1 match { case OptV(a) => a case _ => "no match" })
    t("OptNullV")(1 match { case OptNullV(a) => a case _ => "no match" })
    t("TupV")(1 match { case TupV(_, a) => a case _ => "no match" })
    t("TupNullV")(1 match { case TupNullV(_, a) => a case _ => "no match" })
    t("Wrapped")(1 match { case Wrapped(a) => a case _ => "no match" })
    t("NullWrapped")(1 match { case NullWrapped(a) => a case _ => "no match" })
    t("OptWrapper")(1 match { case OptWrapper(a) => a case _ => "no match" })
    t("OptNullWrap")(1 match { case OptNullWrap(a) => a case _ => "no match" })
  }
}
