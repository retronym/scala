//> using options -Werror
object Config {
  final val Enabled = false
  final val Quiet = true
}

class V(val x: Int) extends AnyVal

object Test {
  def level(): Int = 1

  // the `if` is replaced by its `else` branch, of type Nothing
  def describe: String = (if (Config.Enabled) level() else sys.error("disabled")) match {
    case 0 => "off"
    case 1 => "on"
    case _ => "other"
  }

  def narrower: Int = (if (Config.Enabled) 1 else "s") match {
    case "s" => 0
    case 1   => 1
    case _   => 2
  }

  sealed trait T
  case class A() extends T
  case class B() extends T
  def sealedCases: Int = (if (Config.Enabled) A() else B()) match {
    case A() => 0
    case B() => 1
  }

  // the `if` is replaced by its `null` branch, of type Null
  def nullScrutinee(s: String): Int = (if (Config.Quiet) null else s) match {
    case "a"  => 1
    case null => 0
    case _    => 2
  }

  // the match is itself the surviving branch of a reduced `if`
  def nullScrutineeInBranch(s: String): Int = if (Config.Enabled) 9 else (if (Config.Quiet) null else s) match {
    case "a"  => 1
    case null => 0
    case _    => 2
  }
  def narrowerInScrutinee: Int = (if (Config.Quiet) ((if (Config.Enabled) 1 else "s") match {
    case "s" => 5
    case 1   => 6
    case _   => 7
  }) else 2) match {
    case 5 => 50
    case _ => 0
  }
  def threeLevels(s: String): Int =
    if (Config.Enabled) 9
    else if (Config.Quiet) (if (Config.Enabled) 8 else (if (Config.Quiet) null else s) match {
      case "a"  => 1
      case null => 0
      case _    => 2
    })
    else 7
  def threeLevelsInScrutinee: Int = (if (Config.Quiet) (if (Config.Enabled) 0 else ((if (Config.Enabled) 1 else "s") match {
    case "s" => 5
    case 1   => 6
    case _   => 7
  })) else 2) match {
    case 5 => 50
    case _ => 0
  }
  def underNonConstant(b: Boolean, s: String): Int =
    if (b) 9
    else if (Config.Quiet) ((if (Config.Quiet) null else s) match {
      case "a"  => 1
      case null => 0
      case _    => 2
    })
    else 7
  val inLambda: String => Int = s => if (Config.Enabled) 9 else (if (Config.Quiet) null else s) match {
    case "a"  => 1
    case null => 0
    case _    => 2
  }
  val inLambdaScrutinee: Int => Int = i => (if (Config.Quiet) ((if (Config.Enabled) i else "s") match {
    case "s" => 5
    case 1   => 6
    case _   => 7
  }) else 2) match {
    case 5 => 50
    case _ => 0
  }
  def byName(x: => Int): Int = x
  def inByName(s: String): Int = byName(if (Config.Enabled) 9 else (if (Config.Quiet) null else s) match {
    case "a"  => 1
    case null => 0
    case _    => 2
  })
  def inTry(s: String): Int =
    try {
      if (Config.Enabled) 9 else (if (Config.Quiet) null else s) match {
        case "a"  => 1
        case null => 0
        case _    => 2
      }
    } catch { case _: RuntimeException => 7 }
  def inGuard(s: String): Int = s match {
    case t if (if (Config.Enabled) 9 else (if (Config.Quiet) null else t) match {
      case "a"  => 1
      case null => 0
      case _    => 2
    }) == 0 => 1
    case _ => 2
  }

  // a selection on, and a match on a value class of, a `Nothing` branch failed verification
  def length: Int = (if (Config.Enabled) "s" else sys.error("length")).length
  def valueClass: Int = (if (Config.Enabled) new V(1) else sys.error("value class")) match { case v => v.x }

  def main(args: Array[String]): Unit = {
    def run(f: => Any) = println(try f catch { case e: RuntimeException => e.getMessage })
    run(describe)
    println(narrower)
    println(sealedCases)
    println(nullScrutinee("b"))
    run(length)
    run(valueClass)
    println(List(nullScrutineeInBranch("b"), narrowerInScrutinee, threeLevels("b"), threeLevelsInScrutinee, underNonConstant(false, "b"),
      inLambda("b"), inLambdaScrutinee(1), inByName("b"), inTry("b"), inGuard("b")))
  }
}
