class Broken {
  def test = {
    val capture = "hi there"
    () => {
      final class X extends DelayedInit {
        (() => println(capture)).apply()
        def delayedInit(body: => Unit) = body
      }
      new X
    }
  }
}

object Test {
  def main(args: Array[String]) {
    new Broken().test.apply()
  }
}
