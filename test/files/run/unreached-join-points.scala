//> using options -opt:none

// Codegen omits code at positions it can prove are unreached. The positions below must
// verify and run correctly, in particular the entries of exception handlers.
object Test {
  def bothBranchesReturn(x: Int): Int = {
    val r: Int = if (x > 0) return 1 else return 2
    r
  }

  def matchInStat(t: (Int, Int)): Int = {
    t match { case (a, b) if a > b => return a; case (_, b) => return b }
  }

  def catchAfterJump(x: Int): Int =
    try { if (x > 0) return 1 else throw new Exception("e") }
    catch { case _: Exception => 2 }

  def handlerAfterReturn(x: Int): String =
    try return x.toString
    catch { case e: Exception => e.getMessage }
    finally println("finally")

  def nothingArg(x: Int): Int = {
    def fail(): Nothing = throw new IllegalArgumentException("fail")
    try Integer.max(x, fail())
    catch { case _: IllegalArgumentException => -1 }
  }

  def switchAllJump(x: Int): Int = {
    (x: @scala.annotation.switch) match {
      case 1 => return 10
      case 2 => return 20
      case _ => return 30
    }
  }

  def loopThenThrow(n: Int): Int = {
    var i = 0
    while (i < n) { if (i == 3) return i; i += 1 }
    throw new RuntimeException("end")
  }

  def unitIf(x: Int): Unit = {
    if (x > 0) { println("pos"); return } else { println("neg"); return }
  }

  def main(args: Array[String]): Unit = {
    println(bothBranchesReturn(1)); println(bothBranchesReturn(-1))
    println(matchInStat((2, 1))); println(matchInStat((1, 2)))
    println(catchAfterJump(1)); println(catchAfterJump(-1))
    println(handlerAfterReturn(1))
    println(nothingArg(1))
    println(switchAllJump(1)); println(switchAllJump(2)); println(switchAllJump(3))
    println(loopThenThrow(5))
    try loopThenThrow(1) catch { case e: RuntimeException => println(e.getMessage) }
    unitIf(1); unitIf(-1)
  }
}
