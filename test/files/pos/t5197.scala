object B {
  val x: Int = List("a")
  implicit object O2 extends (List[String] => Int) { def apply(x: List[String]): Int = ??? }
}
