// RefChecks validates the overriding pairs of all base classes in every class, also pairs whose
// members both come from the superclass. Skipping those is unsound: these two pairs only become
// overriding pairs in the subclass, so only the subclass's check reports them.
// See the header of transform/OverridingPairs.scala.

// 1. a type argument of the superclass
class A1[X] { def f(x: X): Int = 1 }
class B1[X] extends A1[X] { def f(x: String): Int = 2 }   // overloads in B1
class C1 extends B1[String]                                 // a pair in C1

// 2. a later mixin that fixes an abstract type
abstract class A2 { type T; def g(x: T): Int = 1 }
abstract class B2 extends A2 { def g(x: Int): Int = 2 }     // overloads in B2
trait M2 { type T = Int }
class C2 extends B2 with M2                                 // a pair in C2
