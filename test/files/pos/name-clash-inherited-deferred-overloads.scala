// Two deferred overloads with the same erasure, inherited from different parents, next to a third
// overload declared in the class. Only Java can implement `M`; the declaration is accepted.
trait H1 { def f(x: List[Int]): String }
trait H2 { def f(x: List[String]): String }

abstract class M extends H1 with H2 { def f(x: Int): String }
