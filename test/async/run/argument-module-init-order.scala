//> using options -Xasync

import scala.concurrent._, duration._
import scala.tools.testkit.async.Async.{async, await}
import ExecutionContext.Implicits.global

object Cfg { var mode = "initial" }
class Holder { object Snapshot { println("Snapshot init, mode = " + Cfg.mode); val mode = Cfg.mode } }

// An object selected in an argument is initialised before an awaiting argument to its right.
object Test {
  def pair(a: String, b: Int) = s"$a/$b"
  def main(args: Array[String]): Unit = {
    val h = new Holder
    val r = Await.result(async { pair(h.Snapshot.mode, await(Future.successful { println("awaited arg"); Cfg.mode = "changed"; 1 })) }, 10.seconds)
    println(r)
  }
}
