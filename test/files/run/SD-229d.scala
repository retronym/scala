class Broken {
  def test = {
    val capture = "hi there"
    () => {
      trait X extends DelayedInit {
        println(capture)
        def delayedInit(body: => Unit) = body
      }
      new X {}
    }
  }
}

object Test {
  def main(args: Array[String]) {
    new Broken().test.apply()
  }
}