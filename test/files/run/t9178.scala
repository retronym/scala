trait Sam { def apply(): Unit }
object Test {

  def main(args: Array[String]): Unit = {
    def foo(): Sam = new Sam { def apply(): Unit = println("apply") }
    val f: Sam = foo /* no parens */ 
    // printed nothing in 2.11.5 as the RHS of the previous line expanded to `val f: Sam = Sam { def apply() = foo() }`
    f()
  }  
}
