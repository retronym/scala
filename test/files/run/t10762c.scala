import language.higherKinds

trait Matcher[-T]{
  def and[U, TC1[_]](other: MatcherFactory1X[U, TC1]): MatcherFactory1X[T with U, TC1]
  class X {
  	def be = {
  	  val temp: MatcherFactory1X[Any,SortableX] = null
  	  and(temp)
  	}
  }
}
trait SortableX[A] {
}

trait MatcherFactory1X[-SC, TC1[_]]


import scala.tools.partest._

object Test extends DirectTest {

  override def extraSettings: String = s"-usejavacp -cp ${testOutput}"

  override def code = ""
  override def show(): Unit = {
    val g = newCompiler()
    import g._
    new Run
    val X = typeOf[Matcher[_]].decl(TypeName("X"))
    val be = X.info.decl(TermName("be"))
    // Was:
    // scala.reflect.internal.FatalError:
    //   Something is wrong: cannot find A in applied type Matcher[T]
    // sought  A in Matcher
    //   classSym  Matcher in <empty>
    //   tparams  T in Matcher
    println(X.thisType.memberInfo(be))
  }
}
