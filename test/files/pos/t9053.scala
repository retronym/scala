class C {
  @volatile var x: Any = _
}

object Test {
  val c = new C
  c.x
}
