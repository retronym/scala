// The null test on the sequence selected by a sequence pattern can't fail for a value class's
// unboxed representation, but it can for a box at a generic position, or for a reference.
// The check file is the output of a compiler that always emits the test, except for the last three lines:
// a `get` declared to return a value class is now evaluated once less, as the test itself no longer calls it.
import collection.SeqFactory.UnapplySeqWrapper
import Array.{UnapplySeqWrapper => ArrayWrapper}

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

// The same for the wrapper that `Array.unapplySeq` returns
object ArrWrapped     { def unapplySeq(a: Int) = new ArrayWrapper[String](Array("x")) }
object ArrNullWrapped { def unapplySeq(a: Int) = new ArrayWrapper[String](null) }
object ArrOptWrapper  { def unapplySeq(a: Int): Option[ArrayWrapper[String]] = Some(new ArrayWrapper[String](Array("x"))) }
object ArrOptNullWrap { def unapplySeq(a: Int): Option[ArrayWrapper[String]] = Some(null).asInstanceOf[Option[ArrayWrapper[String]]] }

// The type of `get` as declared, rather than as seen from the extractor's result, decides whether it returns a box
object Nulls { def box[A]: A = null.asInstanceOf[A] }

class GenVal[A](val a: A) extends AnyVal { def isEmpty = false; def get: A = a }
object GenValV     { def unapplySeq(a: Int) = new GenVal[V](new V(List("x"))) }
object GenValNullV { def unapplySeq(a: Int) = new GenVal[V](Nulls.box[V]) }
object GenValNullW { def unapplySeq(a: Int) = new GenVal[ArrayWrapper[String]](Nulls.box[ArrayWrapper[String]]) }

class GenRef[A](a: A) { def isEmpty = false; def get: A = a }
object GenRefV     { def unapplySeq(a: Int) = new GenRef[V](new V(List("x"))) }
object GenRefNullV { def unapplySeq(a: Int) = new GenRef[V](Nulls.box[V]) }

abstract class Member { type X; def x: Any; def isEmpty = false; def get: X = x.asInstanceOf[X] }
class MemberV(val x: Any) extends Member { type X = V }
object MemberOk   { def unapplySeq(a: Int) = new MemberV(new V(List("x"))) }
object MemberNull { def unapplySeq(a: Int) = new MemberV(null) }

// An abstract type bounded by a value class erases to that value class, so casting null to it is the NPE
class Bounded[A <: V](a: Any) { def isEmpty = false; def get: A = a.asInstanceOf[A] }
object BoundedOk   { def unapplySeq(a: Int) = new Bounded[V](new V(List("x"))) }
object BoundedNull { def unapplySeq(a: Int) = new Bounded[V](null) }

// `get` overriding one with a generic result is declared to return the value class, though the inherited one is not
class Overriding extends GenRef[V](Nulls.box[V]) { override def get: V = new V(List("x")) }
object OverridingOk    { def unapplySeq(a: Int) = new Overriding }
object OverridingUpcast { def unapplySeq(a: Int): GenRef[V] = new GenRef[V](Nulls.box[V]) { override def isEmpty = false } }

// The result of a `get` declared to return a value class
class Declared(val v: V) { def isEmpty = false; def get: V = v }
object DeclaredOk      { def unapplySeq(a: Int) = new Declared(new V(List("x"))) }
object DeclaredNullInV { def unapplySeq(a: Int) = new Declared(new V(null)) }
class DeclaredVal(val xs: List[String]) extends AnyVal { def isEmpty = false; def get: V = new V(xs) }
object DeclaredValOk   { def unapplySeq(a: Int) = new DeclaredVal(List("x")) }
class EmptyParens { def isEmpty = false; def get(): V = new V(List("x")) }
object EmptyParensOk   { def unapplySeq(a: Int) = new EmptyParens }
class Overloaded { def isEmpty = false; def get: V = new V(List("x")); def get(i: Int): Int = i }
object OverloadedOk    { def unapplySeq(a: Int) = new Overloaded }
class Aliased { type X = V; def isEmpty = false; def get: X = new V(List("x")) }
object AliasedOk       { def unapplySeq(a: Int) = new Aliased }

// `get` is declared to return a value class, but that value class is not the sequence
class ProdGet[A](val a: A) extends AnyVal { def _1 = 1; def _2: A = a }
class HasProd[A](a: A) { def isEmpty = false; def get: ProdGet[A] = new ProdGet(a) }
object ProdOk   { def unapplySeq(a: Int) = new HasProd[V](new V(List("x"))) }
object ProdNull { def unapplySeq(a: Int) = new HasProd[V](Nulls.box[V]) }

// `get` of a case class is not what selects the sequence
case class VarGet(xs: String*) { def get = new V(List("get")) }
object VarGet2 { val nullVarargs = new VarGet(null.asInstanceOf[Seq[String]]: _*) }

// A generic `unapplySeq` returns a box, though its result type here is a value class. Unboxing it into the binder is the NPE
class GenRes[A](r: Any) { def unapplySeq(a: Int): A = r.asInstanceOf[A] }
object GenResWOk   extends GenRes[UnapplySeqWrapper[String]](new UnapplySeqWrapper[String](List("x")))
object GenResWNull extends GenRes[UnapplySeqWrapper[String]](null)
object GenResANull extends GenRes[ArrayWrapper[String]](null)
object GenResVOk   extends GenRes[V](new V(List("x")))
object GenResVNull extends GenRes[V](null)

// `get` as declared in a refinement or a mixin isn't the method that's called
object RefinedAnon { def unapplySeq(a: Int) = new GenRef[V](Nulls.box[V]) { override def get: V = super.get; def extra = 1 } }
trait TGet { def get: V }
class MixGet extends GenRef[V](Nulls.box[V]) with TGet
object MixNull { def unapplySeq(a: Int) = new MixGet }
object MixUp   { def unapplySeq(a: Int): GenRef[V] with TGet = new MixGet }

// A bridge between a generic `get` and one declared to return the value class
trait HasGet[A] { def isEmpty = false; def get: A }
class ImplGet(a: Any) extends HasGet[V] { def get: V = a.asInstanceOf[V] }
object BridgeNull { def unapplySeq(a: Int) = new ImplGet(null) }
object BridgeUp   { def unapplySeq(a: Int): HasGet[V] = new ImplGet(null) }

// Other ways of being declared to return a value class
class SelfV(val xs: List[String]) extends AnyVal {
  def isEmpty = false
  def get: this.type = this
  def lengthCompare(n: Int) = xs.lengthCompare(n)
  def apply(i: Int) = xs(i)
  def drop(n: Int) = xs.drop(n)
  def toSeq = xs
}
object SelfOk     { def unapplySeq(a: Int) = new SelfV(List("x")) }
object SelfNullIn { def unapplySeq(a: Int) = new SelfV(null) }
class ValGet(val get: V) { def isEmpty = false }
object ValGetOk   { def unapplySeq(a: Int) = new ValGet(new V(List("x"))) }
object ValGetNull { def unapplySeq(a: Int) = new ValGet(Nulls.box[V]) }
object AliasW     { type W = UnapplySeqWrapper[String]; def unapplySeq(a: Int): W = new UnapplySeqWrapper[String](null) }

// `isEmpty` is still what guards `get`, and a null extractor result is still the NPE
class EmptyDecl { var n = 0; def isEmpty = true; def get: V = { n += 1; new V(List("x")) } }
object EmptyD     { val c = new EmptyDecl; def unapplySeq(a: Int) = c }
object NullResult { def unapplySeq(a: Int): Declared = null }

// A `get` written by the user can tell how many times it is evaluated
class Counting { var n = 0; def isEmpty = false; def get: V = { n += 1; new V(List(s"$n")) } }
object Counted { val c = new Counting; def unapplySeq(a: Int) = c }
class Next(it: Iterator[V]) { def isEmpty = false; def get: V = it.next() }
object Stateful { val n = new Next(Iterator("first", "second", "third").map(s => new V(List(s)))); def unapplySeq(a: Int) = n }
object CountVal {
  var n = 0
  class Counts(val u: String) extends AnyVal { def isEmpty = false; def get: V = { n += 1; new V(List(u)) } }
  def unapplySeq(a: Int) = new Counts("x")
}

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

    t("NullList")((null: List[String]) match { case List(a) => a case _ => "no match" })
    t("AnyNullList")((null: Any) match { case List(a) => a.toString case _ => "no match" })

    t("ArrWrapped")(1 match { case ArrWrapped(a) => a case _ => "no match" })
    t("ArrNullWrapped")(1 match { case ArrNullWrapped(a) => a case _ => "no match" })
    t("ArrOptWrapper")(1 match { case ArrOptWrapper(a) => a case _ => "no match" })
    t("ArrOptNullWrap")(1 match { case ArrOptNullWrap(a) => a case _ => "no match" })
    t("Array")(Array("x") match { case Array(a) => a case _ => "no match" })
    t("ArrayStar")(Array("x", "y") match { case Array(a, _*) => a case _ => "no match" })
    t("ArrayEmpty")(Array[String]() match { case Array() => "empty" case _ => "no match" })
    t("ArrayLength")(Array("x", "y") match { case Array(a) => a case _ => "no match" })
    t("NullArray")((null: Array[String]) match { case Array(a) => a case _ => "no match" })
    t("AnyNullArray")((null: Any) match { case Array(a) => a.toString case _ => "no match" })

    t("GenValV")(1 match { case GenValV(a) => a case _ => "no match" })
    t("GenValNullV")(1 match { case GenValNullV(a) => a case _ => "no match" })
    t("GenValNullW")(1 match { case GenValNullW(a) => a case _ => "no match" })
    t("GenRefV")(1 match { case GenRefV(a) => a case _ => "no match" })
    t("GenRefNullV")(1 match { case GenRefNullV(a) => a case _ => "no match" })
    t("MemberOk")(1 match { case MemberOk(a) => a case _ => "no match" })
    t("MemberNull")(1 match { case MemberNull(a) => a case _ => "no match" })
    t("BoundedOk")(1 match { case BoundedOk(a) => a case _ => "no match" })
    t("BoundedNull")(1 match { case BoundedNull(a) => a case _ => "no match" })
    t("OverridingOk")(1 match { case OverridingOk(a) => a case _ => "no match" })
    t("OverridingUpcast")(1 match { case OverridingUpcast(a) => a case _ => "no match" })

    t("DeclaredOk")(1 match { case DeclaredOk(a) => a case _ => "no match" })
    t("DeclaredNullInV")(1 match { case DeclaredNullInV(a) => a case _ => "no match" })
    t("DeclaredValOk")(1 match { case DeclaredValOk(a) => a case _ => "no match" })
    t("EmptyParensOk")(1 match { case EmptyParensOk(a) => a case _ => "no match" })
    t("OverloadedOk")(1 match { case OverloadedOk(a) => a case _ => "no match" })
    t("AliasedOk")(1 match { case AliasedOk(a) => a case _ => "no match" })

    t("ProdOk")(1 match { case ProdOk(_, a) => a case _ => "no match" })
    t("ProdNull")(1 match { case ProdNull(_, a) => a case _ => "no match" })

    t("VarGetOk")(VarGet("x") match { case VarGet(a) => a case _ => "no match" })
    t("VarGetNull")(VarGet2.nullVarargs match { case VarGet(a) => a case _ => "no match" })

    t("GenResWOk")(1 match { case GenResWOk(a) => a case _ => "no match" })
    t("GenResWNull")(1 match { case GenResWNull(a) => a case _ => "no match" })
    t("GenResANull")(1 match { case GenResANull(a) => a case _ => "no match" })
    t("GenResVOk")(1 match { case GenResVOk(a) => a case _ => "no match" })
    t("GenResVNull")(1 match { case GenResVNull(a) => a case _ => "no match" })

    t("RefinedAnon")(1 match { case RefinedAnon(a) => a case _ => "no match" })
    t("MixNull")(1 match { case MixNull(a) => a case _ => "no match" })
    t("MixUp")(1 match { case MixUp(a) => a case _ => "no match" })
    t("BridgeNull")(1 match { case BridgeNull(a) => a case _ => "no match" })
    t("BridgeUp")(1 match { case BridgeUp(a) => a case _ => "no match" })

    t("SelfOk")(1 match { case SelfOk(a) => a case _ => "no match" })
    t("SelfNullIn")(1 match { case SelfNullIn(a) => a case _ => "no match" })
    t("ValGetOk")(1 match { case ValGetOk(a) => a case _ => "no match" })
    t("ValGetNull")(1 match { case ValGetNull(a) => a case _ => "no match" })
    t("AliasW")(1 match { case AliasW(a) => a case _ => "no match" })

    t("EmptyD")(1 match { case EmptyD(a) => a case _ => s"no match, ${EmptyD.c.n} evaluations" })
    t("NullResult")(1 match { case NullResult(a) => a case _ => "no match" })

    // The length guards, with each wrapper around null
    t("WrapEmpty")(1 match { case NullWrapped() => "empty" case _ => "no match" })
    t("WrapStar")(1 match { case NullWrapped(a, _*) => a case _ => "no match" })
    t("WrapOnlyStar")(1 match { case NullWrapped(_*) => "star" case _ => "no match" })
    t("ArrWrapEmpty")(1 match { case ArrNullWrapped() => "empty" case _ => "no match" })
    t("ArrWrapStar")(1 match { case ArrNullWrapped(a, _*) => a case _ => "no match" })
    t("ArrWrapOnlyStar")(1 match { case ArrNullWrapped(_*) => "star" case _ => "no match" })

    // A null scrutinee at a nested position, and through the other factories
    t("SomeNullList")((Some(null): Option[List[String]]) match { case Some(List(a)) => a case _ => "no match" })
    t("ListOfNullList")(List[List[String]](null) match { case List(List(a)) => a case _ => "no match" })
    t("TupNullArr")(((null: Array[String]), 1) match { case (Array(a), _) => a case _ => "no match" })
    t("SeqNull")((null: collection.Seq[String]) match { case collection.Seq(a) => a case _ => "no match" })
    t("VectorNull")((null: Vector[String]) match { case Vector(a) => a case _ => "no match" })

    t("Counted")(1 match { case Counted(a) => s"$a, ${Counted.c.n} evaluations" case _ => "no match" })
    t("Stateful")(1 match { case Stateful(a) => a case _ => "no match" })
    t("CountVal")(1 match { case CountVal(a) => s"$a, ${CountVal.n} evaluations" case _ => "no match" })
  }
}
