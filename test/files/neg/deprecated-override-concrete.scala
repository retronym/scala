//> using options -Werror -deprecation

import scala.annotation.nowarn

class A {
  def f = 1
  val v = 1
  def g = 1
  def h = 1
  @deprecated("old", "1.0") def old = 1
  @deprecatedOverriding("do not override", "1.0") def o = 1
}

abstract class Abs {
  def d: Int
}

class B extends A {
  @deprecated("no longer", "1.0") override def f = 2    // warn
  @deprecated("no longer", "1.0") override val v = 2    // warn
  @deprecated("no longer", "1.0") override def old = 2  // nowarn, the overridden member is deprecated

  // nowarn for the reference to the deprecated member, which is in a deprecated definition
  @deprecated("no longer", "1.0") def other = old

  // warn for the override, nowarn for the reference
  @deprecated("no longer", "1.0") override def g = old

  @nowarn("cat=deprecation")
  @deprecated("no longer", "1.0") override def h = 2    // nowarn
}

class C extends Abs {
  @deprecated("no longer", "1.0") override def d = 2    // nowarn, the overridden member is deferred
}
