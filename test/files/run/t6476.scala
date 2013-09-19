object Test extends App {
  val x = 0
  assert(s"$"$x$"" == "\"0\"")
  assert(s"""$"$x$"""" == "\"0\"")
}
