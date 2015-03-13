object Test {
  trait Command
  object Command {
    sealed trait Execution extends Command
  }

  class Buzz() extends Command.Execution
  class Door() extends Command.Execution

  def foo(cmd: Command.Execution) = cmd match {
    case x @ (_: Buzz) => "buzz"
    case x @ (_: Door) => "door"
  }

  def main(args: Array[String]): Unit = {
    assert(foo(new Buzz()) == "buzz")
    assert(foo(new Door()) == "door")
  }
}
