// An override may specialize a type parameter that the overridden method does not. `CT` has the
// variant `ap$mIc$sp` for `B = Int`; the only variant of `CH` is `ap$mIIc$sp`, for `A = Int, B = Int`.
class CT { def ap[A, @specialized(Int) B](a: A, b: B): String = "CT" }
class CH extends CT {
  override def ap[@specialized(Int) A, @specialized(Int) B](a: A, b: B): String = "CH"
}
class CI extends CH {
  override def ap[@specialized(Int) A, @specialized(Int) B](a: A, b: B): String = "CI"
}
// The override is in a trait, which does not get the variant: the class that mixes the trait in does.
class EH extends TH
class EI extends TI
class EJ extends TH with TJ
class EK extends TK[String]
// Two traits that override the method and are unrelated to each other: the later one decides.
class EK2 extends TK[String] with TK2[String]
class EK3 extends TK[String] with TS[String]
class EK4 extends TS[String] with TK[String]
abstract class AE extends TH
class EA extends AE
// `CO` has a variant named `ap$mIc$sp`, but for the other alternative.
class TO {
  def ap[A, @specialized(Int) B](a: A, b: B): String = "TO2"
  def ap[A, @specialized(Int) B](a: A, b: B, c: Int): String = "TO3"
}
class CO extends TO {
  override def ap[@specialized(Int) A, @specialized(Int) B](a: A, b: B): String = "CO2"
  override def ap[A, @specialized(Int) B](a: A, b: B, c: Int): String = "CO3"
}
abstract class AT { def ap[A, @specialized(Int) B](a: A, b: B): String }
class AH extends AT {
  def ap[@specialized(Int) A, @specialized(Int) B](a: A, b: B): String = "AH"
}
object Test {
  def t(b: => Any): Unit = println(try b catch { case e: Throwable => e.toString })
  def main(args: Array[String]): Unit = {
    t((new CH: CT).ap("a", "b")) // CH
    t((new CH: CT).ap("a", 1))   // CT: the override is skipped
    t((new CH: CT).ap(1, 1))     // CT
    t((new AH: AT).ap("a", "b")) // AH
    t((new AH: AT).ap("a", 1))   // RuntimeException: this should never be called
    t((new CI: CT).ap("a", 1))
    t((new CI: CH).ap(1, 1))
    t((new TH {}: TT).ap("a", 1))
    t((new TH {}: TT).ap(1L, 2L))
    for (x <- List[TT](new EH, new EI, new EJ, new EA)) {
      t(x.ap("a", 1))
      t(x.ap(1L, 2L))
    }
    t((new EK: TG[String]).ap("a", 1, "x"))
    t((new EK: TG[String]).ap(1, 1, "x"))
    for (x <- List[TG[String]](new EK2, new EK3, new EK4)) {
      t(x.ap("a", 1, "x"))
      t(x.ap(1, 1, "x"))
    }
    t((new CO: TO).ap("a", 1))
    t((new CO: TO).ap("a", 1, 2))
  }
}
