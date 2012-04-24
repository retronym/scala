import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

// IdTypeConstructorTest
object Test extends ScaladocModelTest {

  override def code = """
        package scala.test {
          import language._
          object Alias {
         	  type Id[X] = X
         	  type S = String
          }

          class A[F[_]] {
            def id[A](x: F[A]): F[A] = ???
            def s: F[Alias.S] = ???
          }

          object IdA extends A[Alias.Id]
        }
    """

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    import access._

    val idAObject = rootPackage._package("scala")._package("test")._object("IdA")
    val idMethod = idAObject._method("id")
    println(idMethod.resultType.name)
    val sMethod = idAObject._method("s")
    println(sMethod.resultType.name)
  }
}
