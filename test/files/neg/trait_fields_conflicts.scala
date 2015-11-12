// TODO: nice error instead of crash
trait OneVal { val x: Int = 123 }
class Conflicting extends OneVal { def x: Int = 1 }

// variation:
// trait OneVal[T] { val x: Int = 123 }
// class OverridingVal extends OneVal[Int] { def x: Int = 1 }