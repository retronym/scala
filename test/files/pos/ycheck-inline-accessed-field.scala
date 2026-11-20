//> using options -Ycheck:_ -Werror

// SuperAccessors expands the name of a private field that an @inline method reads. Re-typing the
// field's definition names it after its symbol, which -Ycheck used to report as "ValDef i differs".
class A {
  private[this] var i = 0
  @inline def f = i
}
