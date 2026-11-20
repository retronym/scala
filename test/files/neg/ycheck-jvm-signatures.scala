//> using options -Ycheck:jvm -Werror

// -Ycheck:jvm reports a generic signature whose erasure, as Java computes it, is not the descriptor.
// Here the signature says `U` (bounded by Object) where the descriptor has `float`.
class C {
  def max[U <: Float](x: U, y: U): U = x
}
