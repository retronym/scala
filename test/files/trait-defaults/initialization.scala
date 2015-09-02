trait T {
  lazy val laze = {println("a"); 1}

  val a = 1
  println("")
  val b = 2
  println(a+b)
}


class C extends T