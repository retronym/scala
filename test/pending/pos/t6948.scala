import collection.generic._
import collection.immutable.WrappedString

object Test {
  def shuffle[T, CC[X] <: TraversableOnce[X]]
             (xs: CC[T])
             (implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = null.asInstanceOf[CC[T]]

  shuffle("": WrappedString)                   // fail (2.10.0)
  shuffle(1 until 2)                           // fail (2.10.0)


  def foo1(r: R1) = shuffle(r)                  // fail (2.9.2, 2.10.0)
  def foo2(r: R1) = shuffle(r: IndexedSeq[Int]) // okay
  def foo3(r: R2) = shuffle(r)                  // okay

  abstract class AbstractSeq[+A] extends collection.Seq[A]

  abstract class R1 extends AbstractSeq[Int] with IndexedSeq[Int]

  abstract class R2 extends IndexedSeq[Int]
}

