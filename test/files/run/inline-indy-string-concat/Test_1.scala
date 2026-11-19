//> using jvm 11+
//> using options -opt:inline:** -release 11 -Wopt -Werror

// Methods containing an invokedynamic of StringConcatFactory (makeConcatWithConstants from
// scalac, makeConcat from javac with -XDstringConcat=indy) or of scala.runtime.SymbolLiteral
// can be inlined into other classes. -Wopt -Werror makes a failure to inline an error.

package p {
  object S {
    // The descriptor of the string concatenation names the package-private p.Hidden, so this
    // method cannot be inlined outside of package p.
    @inline final def show(): String = "h=" + Q.H
  }
}

class K {
  @inline final def greet(n: Int): String = "n=" + n + "!"
  @inline final def sym: Symbol = Symbol("foo")
}

object Test {
  import scala.annotation.nowarn

  def t1(k: K) = k.greet(3)
  def t2 = (J.cat(4): @inline)
  def t3(k: K) = k.sym
  @nowarn("cat=optimizer") @noinline
  def t4 = p.S.show()

  def main(args: Array[String]): Unit = {
    println(t1(new K))
    println(t2)
    println(t3(new K) eq Symbol("foo"))
    println(t4)
  }
}
