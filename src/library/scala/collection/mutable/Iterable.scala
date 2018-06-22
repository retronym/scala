package scala.collection.mutable

import scala.collection.IterableFactory
import scala.language.higherKinds

trait Iterable[A]
  extends collection.Iterable[A]
    with IterableOps[A, Iterable, Iterable[A]] {

  override def iterableFactory: IterableFactory[IterableCC] = Iterable
}

/**
  * @define coll mutable collection
  * @define Coll `mutable.Iterable`
  */
trait IterableOps[A, +CC[X], +C]
  extends collection.IterableOps[A, CC, C] {

}

/**
  * $factoryInfo
  * @define coll mutable collection
  * @define Coll `mutable.Iterable`
  */
@SerialVersionUID(3L)
object Iterable extends IterableFactory.Delegate[Iterable](ArrayBuffer)

/** Explicit instantiation of the `Iterable` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractIterable[A] extends scala.collection.AbstractIterable[A] with Iterable[A]
