trait TraversableLike {
  type A
  type Repr
  def tail: Repr = null.asInstanceOf[Repr]
}

abstract class AbstractTrav[AA] extends TraversableLike {
  type A = AA
  type Repr = AbstractTrav[AA]
}

class C[A] extends AbstractTrav[A]
