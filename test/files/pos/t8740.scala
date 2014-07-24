trait SomeTag

trait Test {  
  type @@[+A, T] = A with T
  def withSomeTag[A](a: A): @@[A, SomeTag]

  type TypeAlias = @@[String, SomeTag]
  
  // okay, type arg inference compares `@@[?A, SomeTag] <:< @@[String, A]`
  // we compare the type args pairwise and only constrain `?A` with `<: String`.
  withSomeTag("foo"): @@[String, SomeTag]

  // so the inferred call ends up as
  withSomeTag[String]("foo") : @@[String, SomeTag]

  // the same type arg naturally also leads to a well typed:
  withSomeTag[String]("foo") : TypeAlias

  // but, we can't infer the type argument in this case!
  // Why? Subtyping calls `normalize` on LHS and RHS and as such
  // goes from `TypeAlias` all the way to `String with SomeTag`
  withSomeTag("foo") : TypeAlias

  // We can expand those ourselves to provoke the same inference
  // failure:
  def withSomeTag2[A](a: A): A with SomeTag
  withSomeTag2("foo") : String with SomeTag

  // Why does that fail?
  // (?A with SomeTag) <:< (String with SomeTag)
  //   ((?A with SomeTag) <:< String) && ((?A with SomeTag) <:< SomeTag)
  //   (?A <:< String || SomeTag <:< String) && (?A <:< SomeTag || SomeTag <:< SomeTag)
  //
  // ?A <:< SomeTag is true, but overconstrains the type variable.
  //
  // The fix for this bug reorders the conditions to try parent types including
  // type variables only after trying the other conditions.
  //
  // (SomeTag <:< String || ?A <:< String) && (SomeTag <:< SomeTag || ?A <:< SomeTag
}


// a variation on the theme in which the type variable ?A won't
// directly appear as a parent of a refined type, but rather is
// wrapped in Option[?A].
trait Test2 {  
  type @@[+A, T] = A with T
  def withSomeTag[A](a: A): @@[Option[A], SomeTag]

  // but this not:
  type TypeAlias = @@[Option[String], SomeTag]
  
  // okay
  withSomeTag("foo"): @@[Option[String], SomeTag]
  withSomeTag[String]("foo") : TypeAlias
  withSomeTag("foo") : TypeAlias
}
