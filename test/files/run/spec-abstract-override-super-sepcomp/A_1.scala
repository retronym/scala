abstract class B5[@specialized(Int) T] { def m(t: T): T }
class C5 extends B5[Int] { def m(t: Int) = t }
class B7[@specialized(Int) T] { def m(t: T): T = t }
