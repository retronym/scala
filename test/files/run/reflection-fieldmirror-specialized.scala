import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

class Spec[@specialized(Int) T](val t: T)
class SpecVar[@specialized(Int) T](var t: T)
class Spec2[@specialized(Int, Double) A, @specialized(Int, Double) B](val a: A, var b: B)
class Sub extends Spec[Int](5)

object Test extends App {
  def field(tpe: Type, name: String) = tpe.decl(TermName(name + " ")).asTerm
  def getter(tpe: Type, name: String) = tpe.decl(TermName(name)).asTerm

  def test(obj: Any, tpe: Type, name: String, newValue: Any, read: () => Any): Unit = {
    val im = cm.reflect(obj)
    val fm = im.reflectField(field(tpe, name))
    println(s"${obj.getClass.getName}.$name")
    println(s"  get: ${fm.get}")
    println(s"  get via accessor symbol: ${im.reflectField(getter(tpe, name)).get}")
    fm.set(newValue)
    println(s"  get after set: ${fm.get}")
    println(s"  read after set: ${read()}")
  }

  val specInt = new Spec[Int](3)
  test(specInt, typeOf[Spec[_]], "t", 4, () => specInt.t)

  val specString = new Spec[String]("three")
  test(specString, typeOf[Spec[_]], "t", "four", () => specString.t)

  val specVarInt = new SpecVar[Int](3)
  test(specVarInt, typeOf[SpecVar[_]], "t", 4, () => specVarInt.t)

  val spec2 = new Spec2[Int, Double](1, 2.0)
  test(spec2, typeOf[Spec2[_, _]], "a", 10, () => spec2.a)
  test(spec2, typeOf[Spec2[_, _]], "b", 20.0, () => spec2.b)

  val spec2Generic = new Spec2[String, Int]("a", 2)
  test(spec2Generic, typeOf[Spec2[_, _]], "a", "aa", () => spec2Generic.a)
  test(spec2Generic, typeOf[Spec2[_, _]], "b", 20, () => spec2Generic.b)

  val sub = new Sub
  test(sub, typeOf[Spec[_]], "t", 6, () => sub.t)

  // a field mirror bound to another receiver resolves the field for that receiver
  val fm = cm.reflect(specString).reflectField(field(typeOf[Spec[_]], "t"))
  println(fm.bind(new Spec[Int](7)).get)
  println(fm.bind(new Spec[String]("seven")).get)
}
