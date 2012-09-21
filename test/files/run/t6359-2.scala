class M(val t: Int) extends AnyVal {
  def lazyString = {
    object X {
      println("X!" + t)
    }
    class Y {
      println("Y!" + t)
    }

    () => {X; new Y}
  }
}

object Test extends App {
  new M(1).lazyString.apply()
}
