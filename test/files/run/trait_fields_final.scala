// TODO: bytecode test

// TODO: clarify meaning of final in traits
// In the new compiler, there's no final modifier after mixin for `meh`'s setter,
// whereas 2.12.0-M3 makes meh's trait setter final.
// NOTE: bytecode is identical, but the scalasignature is different
trait Foo { self: Meh =>
  def bar(x: String) = x == "a"
  private final val meh = bar("a")
}

abstract class Meh extends Foo
