import collection.generic.Subtractable

trait CoSet[U, +A <: U]
  extends CoSetLike[U, A, ({type S[A1 <: U] = CoSet[U, A1]})#S]

trait CoSetLike[U, +A <: U, +This[X] <: CoSetLike[U, A, This] with CoSet[U, A]]
  extends Subtractable[U, This[U]] {
  implicitly[CoSet[U, U] => String]
}
