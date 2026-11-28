import scala.language.existentials

class Ref[T](var v: T)
class Inv[T]

object Test {
  def copyFirstToSecond[T](rs: List[Ref[T]]): Unit = rs(1).v = rs(0).v

  def test(loose: List[Ref[_]], pair: (Ref[_], Ref[_]), one: (Ref[_], Any), reg: Map[Class[_], Ref[_]], inv: Inv[Inv[_]]): Unit = {
    // each needs one T with Ref[_] <: Ref[T]
    val t1: List[Ref[T]] forSome { type T } = loose               // error
    val t2: (Ref[T], Ref[T]) forSome { type T } = pair            // error
    val t3: (Ref[T], T) forSome { type T } = one                  // error
    val t4: Map[Class[T], Ref[T]] forSome { type T } = reg        // error
    val t5: Inv[Inv[T]] forSome { type T } = inv                  // error
    copyFirstToSecond(t1)
  }

  def ok(same: List[Ref[Int]], lower: List[Ref[_ >: String]], cov: List[Option[_]], r: Ref[_], rr: (Ref[T], Ref[T]) forSome { type T }): Unit = {
    val o1: List[Ref[T]] forSome { type T } = same
    val o2: List[Ref[_ >: T]] forSome { type T } = lower
    val o3: List[Option[T]] forSome { type T } = cov
    val o4: Ref[T] forSome { type T } = r
    val o5: (Ref[T], Ref[T]) forSome { type T } = rr
    val o6: List[Ref[_]] = same
  }
}
