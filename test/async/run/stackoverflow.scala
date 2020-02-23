import scala.tools.nsc.transform.async.user.AsyncId._

object Test extends App {

  async {
    var i = 100000000
    while (i > 0) {
      if (false) {
        await(())
      }
      i -= 1
    }
  }

}
