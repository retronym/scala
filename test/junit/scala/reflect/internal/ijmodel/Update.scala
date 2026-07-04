package scala.reflect.internal.ijmodel

/** Mirrors psi/types/recursiveUpdate/Update.scala: the element type of a fused
 *  substitution chain.  (Not sealed only because the cases live in their
 *  IJ-mirroring files: `TypeParamSubstitution` in ScSubstitutor.scala,
 *  `ThisTypeSubstitution` in ThisTypeSubstitution.scala.) */
trait Updates { self: IjTypeSystem =>
  trait Update
}
