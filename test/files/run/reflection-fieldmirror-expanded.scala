import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

package pk {
  // The field of a private val gets an expanded name when another class accesses it:
  // here `pk$S$$name`, because the specialized subclass `S$mcI$sp` reads it in `get$mcI$sp`.
  class S[@specialized(Int) T](t: T) {
    private val name: String = "n" + t
    def get(x: T): String = name
  }

  // scala/bug#9306: the nested class makes the field `pk$L$$x`.
  class L {
    private val x = 123
    class In { def get = x }
  }
}

object Test {
  import pk._

  def getter(tpe: Type, name: String): TermSymbol =
    tpe.decl(TermName(name)).alternatives.collectFirst { case m: MethodSymbol if m.isGetter => m }.get

  def show[A](a: A, sym: TermSymbol): Unit = {
    val im = cm.reflect(a)(scala.reflect.ClassTag(a.getClass))
    println(try im.reflectField(sym).get catch { case e: Throwable => e.getClass.getName })
  }

  def main(args: Array[String]): Unit = {
    show(new S[Int](3), getter(typeOf[S[_]], "name"))
    show(new S[String]("s"), getter(typeOf[S[_]], "name"))
    show(new L, getter(typeOf[L], "x"))
    show(new L, typeOf[L].decl(TermName("x ")).asTerm)
  }
}
