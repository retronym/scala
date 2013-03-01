class Lazies {
  val lock = new {}
  object U {
    val __lazyLock = lock
    lazy val v1 = {Thread.sleep(10); 0}
    lazy val v3 = T.v2
  }
  object T {
    val __lazyLock = lock
    lazy val v2 = U.v1
  }
}

object Test extends App {
  import scala.concurrent._, ExecutionContext.Implicits.global, duration._
  def test {
    val l = new Lazies
    val fs = List(() => l.U.v3, () => l.T.v2)
    val futures = fs.map(f => future(f()))
    Await.result(Future.sequence(futures), 10.seconds)
  }
  (1 to 10) foreach (_ => test)
}
