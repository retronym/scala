object Test extends App {
  trait A; trait B
  println((null: A with B).isInstanceOf[A with B])
}
