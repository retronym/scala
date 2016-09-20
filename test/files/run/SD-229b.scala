class Broken {
  def is(ee: AnyRef) = {
    new Delayed {
      println(ee)
    }
  }
}

class Delayed extends DelayedInit {
  def delayedInit(x: => Unit): Unit = x
}

object Test {
  def main(args: Array[String]): Unit = {
    new Broken().is("hello?")
  }
}
