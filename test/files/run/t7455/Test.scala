import scala.tools.partest._

object Test extends DirectTest {
  override def code = ""

  def show {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    val compiler = newCompiler("-cp", classpath, "-d", testOutput.path)
    import compiler._, definitions._
    new compiler.Run
    val classInfo = compiler.rootMirror.staticClass("Vector$PrivateInner").info

    /* This test code used to crash:
    val constructor = classInfo.member(nme.CONSTRUCTOR)
    println(constructor.defString)
    val param1 = constructor.info.params.head
    println(param1.info)
    println(fullyInitializeSymbol(param1))

    OLD OUTPUT:
      private[package <empty>] def <init>(x$2: Vector$1): Vector$PrivateInner
      Vector$1
      class Vector$1
      error: error while loading Vector$1, class file 't7455-run.obj/Vector$1.class' is broken
      (class java.util.NoSuchElementException/key not found: E)
    */

    // this no longer crashes. if we compile under -optimize we see the private constructor
    // otherwise we see no constructors, hence the iteration through alternatives.
    for {
      constr <- classInfo.member(nme.CONSTRUCTOR).alternatives
      param <- constr.info.params
    } fullyInitializeSymbol(param)
  }
}
