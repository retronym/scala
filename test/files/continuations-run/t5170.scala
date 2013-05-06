import scala.util.continuations._

object Test {
  // OK!
  def f1(n:Int):Int @cps[Unit] = {
    shiftUnit0[Int, Unit](3)
  }

  // OK!
  def f2(n:Int):Unit @cps[Unit] = {
    val m = f1(n)
    shiftUnit0[Unit, Unit]()
  }

  // OK!
  def f5(n:Int):Int @cps[String] = {
    shiftUnit0[Int, String](3)
  }

  // OK!
  def f6(n:Int):Unit @cps[String] = {
    val m = f5(n)
    shiftUnit0[Unit, String]()
  }

  // OK!
  def f3(n:Int):Int @cps[Nothing] = {
    shiftUnit0[Int, Nothing](3)
  }

  /*
  <console>:12: error: type mismatch;
 found   : Int => scala.util.continuations.ControlContext[Unit,Nothing,Nothing]
 required: Int => scala.util.continuations.ControlContext[Unit,B1,Nothing]
           val m = f3(n)
               ^
   */
  def f4(n:Int):Unit @cps[Nothing] = {
    val m = f3(n)
    shiftUnit0[Unit, Nothing]()

  }

  def main(args: Array[String]): Unit = {

  }
}
