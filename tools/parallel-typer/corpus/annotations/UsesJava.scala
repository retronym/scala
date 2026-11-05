package ann

@JAnn(level = Level.HIGH, tags = Array("a", "b"), sizes = Array(3))
class UsesJava {
  @JAnn(level = Level.LOW) def m: Int = 1
}
