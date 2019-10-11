import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object Test {
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 1024) {
      println(i)
      val tb = currentMirror.mkToolBox()
      // leaks this toolbox via synthetic "wrapper" packages entered in RootClass!
      val sym = tb.define(
        q"""
          object Test {
            object C { val data = new Array[Byte](16 * 1024 * 1024) }
            class C
            def main(args: Array[String]) { C.data; scala.reflect.runtime.universe.typeOf[C.type] }
          }""")
      tb.eval(q"""$sym.main(null)""")
      // workaround for issue noted https://github.com/scala/bug/issues/6412#issuecomment-540357595
      unlinkFromRootOwner(sym)

      // This version doesn't leak
      // tb.eval(q"""{object Test { object C { val data = new Array[Byte](16 * 1024 * 1024) }; class C; def main(args: Array[String]) { C.data }}; Test.main(null)}""")
    }
    println("sleeping"); Thread.sleep(120 * 1000)
  }
  def unlinkFromRootOwner(sym0: Symbol): Unit = {
    val symtab = universe.asInstanceOf[scala.reflect.internal.SymbolTable]
    val sym = sym0.asInstanceOf[symtab.Symbol]
    val symToRemove = sym.ownerChain.dropRight(1).last.sourceModule
    val scope = symToRemove.owner.info.decls
    scope.asInstanceOf[{def syncLockSynchronized[T](o: => T): T}].syncLockSynchronized {
      println(s"unlinking $symToRemove")
      scope.unlink(symToRemove)
    }
  }
}
