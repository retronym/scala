//> using options -deprecation
//
import scala.tools.partest.Util.ArrayDeep
import scala.language.reflectiveCalls

object Test2 {
  import java.io.{BufferedReader,FileReader, IOException}
  class Reader(fname: String) {
    private val in = new BufferedReader(new FileReader(fname))

    @throws(classOf[IOException])
    def read() = in.read()
  }
  def run: Unit = {
    val method = classOf[Reader].getMethod("read")
    method.getExceptionTypes foreach println
  }
}

/* Java:
public class Main {
    @Deprecated
    public void foo() {}
    public static void main(String[] args) throws Exception {
        Method method = Class.forName("test.Main").getMethod("foo", new Class[]{});
        Annotation annotation = method.getAnnotation(Deprecated.class);
        System.out.println(annotation); // @java.lang.Deprecated()
    }
}
*/
object Test3 {
  import java.lang.Deprecated
  class Foo {
    @Deprecated
    def foo: Unit = ()
  }
  def run: Unit = {
    val method = classOf[Foo].getMethod("foo")
    val annotation = method.getAnnotation(classOf[Deprecated])
    println(annotation)
  }
}

/* Java:
@Retention(value=RetentionPolicy.RUNTIME)
@interface Source {
   public String url();
   public String mail();
}
@Source(url="https://scala.epfl.ch", mail="scala@lists.epfl.ch")
class Foo {}
public class Main {
    public static void main(String[] args) throws Exception {
        Class clazz = Class.forName("test.Foo");
        Annotation[] annotations = clazz.getAnnotations();
        for (int i = 0; i < annotations.length; i++)
            System.out.println(annotations[i]);
        // @test.Main$Source(url=https://scala-lang.org, mail=scala@lists.epfl.ch)
    }
}
*/
object Test4 {
  import test.SourceAnnotation_1
  @SourceAnnotation_1(value = "https://scala-lang.org",
                    mails = Array("scala@lists.epfl.ch", "scala-lounge@lists.epfl.ch"))
  class Foo1
  @SourceAnnotation_1(value = "http://bloodsuckers.com",
                    mails = Array("you@bloodsuckers.com"))
  class Foo2
  @SourceAnnotation_1("http://bloodsuckers.com")
  class Foo3
  class Foo4 {
    @SourceAnnotation_1("file:///dev/null")
    val x = 1
  }
  class Foo5 {
    @SourceAnnotation_1("file:///dev/zero")
    def bar: Int = 0
  }
  class Foo6 @SourceAnnotation_1("primary constructor") (s: String) {
    // to guarantee that primary constructor annotations
    // are not applied to secondary constructors
    def this() = this("")
  }
  class Foo7(s: String) {
    @SourceAnnotation_1("secondary constructor")
    def this() = this("")
  }
  class Foo8(@SourceAnnotation_1("constructor val") val n: Int) {}
  class Foo9 {
    import scala.annotation.meta._
    import scala.beans.BeanProperty
    @(SourceAnnotation_1 @getter)("https://apple.com") val x = 0
    @BeanProperty @(SourceAnnotation_1 @beanSetter)("http://uppla.com") var y = 0

    type myAnn = SourceAnnotation_1 @beanGetter @field
    @BeanProperty @myAnn("http://eppli.com") var z = 0

    type myAnn2[T] = SourceAnnotation_1 @beanGetter @field
    @BeanProperty @myAnn2[String]("http://eppli.com") var z2 = 0

    type myAnn3[CC[_]] = SourceAnnotation_1 @beanGetter @field
    @BeanProperty @myAnn3[List]("http://eppli.com") var z3 = 0
  }
  class Foo10(@SourceAnnotation_1("on param 1") val name: String)
  class Foo11(@(SourceAnnotation_1 @scala.annotation.meta.field)("on param 2") val name: String)
  class Foo12(@(SourceAnnotation_1 @scala.annotation.meta.setter)("on param 3") var name: String)
  def run: Unit = {
    import java.lang.annotation.Annotation
    import java.lang.reflect.AnnotatedElement
    def printSourceAnnotation(a: Annotation): Unit = {
      val ann = a.asInstanceOf[SourceAnnotation_1]
      println("@test.SourceAnnotation_1(mails=" + ann.mails.deep.mkString("{", ",", "}") +
              ", value=" + ann.value + ")")
    }
    def printSourceAnnotations(target: AnnotatedElement): Unit = {
      //print SourceAnnotation in a predefined way to insure
      // against difference in the JVMs (e.g. Sun's vs IBM's)
      val anns = target.getAnnotations()
      anns foreach printSourceAnnotation
      if (anns.length > 0) {
        println(target)
        println()
      }
    }
    def printParamSourceAnnotations(target: { def getParameterAnnotations(): Array[Array[Annotation]] }): Unit = {
      val anns = target.getParameterAnnotations().flatten
      anns foreach printSourceAnnotation
      if (anns.length > 0) {
        println(target)
        println()
      }
    }
    printSourceAnnotations(classOf[Foo1])
    printSourceAnnotations(classOf[Foo2])
    printSourceAnnotations(classOf[Foo3])
    classOf[Foo4].getDeclaredFields  foreach printSourceAnnotations
    classOf[Foo4].getDeclaredMethods foreach printSourceAnnotations
    classOf[Foo5].getDeclaredMethods foreach printSourceAnnotations
    classOf[Foo6].getDeclaredConstructors foreach printSourceAnnotations
    classOf[Foo7].getDeclaredConstructors foreach printSourceAnnotations
    classOf[Foo8].getDeclaredFields  foreach printSourceAnnotations
    classOf[Foo8].getDeclaredMethods foreach printSourceAnnotations
    classOf[Foo8].getDeclaredConstructors foreach printParamSourceAnnotations
    classOf[Foo9].getDeclaredFields.sortWith((x, y) => x.toString < y.toString)  foreach printSourceAnnotations
    classOf[Foo9].getDeclaredMethods.sortWith((x, y) => x.toString < y.toString) foreach printSourceAnnotations
    classOf[Foo10].getDeclaredFields.sortWith((x, y) => x.toString < y.toString)  foreach printSourceAnnotations
    classOf[Foo10].getDeclaredMethods.sortWith((x, y) => x.toString < y.toString) foreach printSourceAnnotations
    classOf[Foo10].getDeclaredConstructors foreach printParamSourceAnnotations
    classOf[Foo11].getDeclaredFields.sortWith((x, y) => x.toString < y.toString)  foreach printSourceAnnotations
    classOf[Foo11].getDeclaredMethods.sortWith((x, y) => x.toString < y.toString) foreach printSourceAnnotations
    classOf[Foo11].getDeclaredConstructors foreach printParamSourceAnnotations
    classOf[Foo12].getDeclaredFields.sortWith((x, y) => x.toString < y.toString)  foreach printSourceAnnotations
    classOf[Foo12].getDeclaredMethods.sortWith((x, y) => x.toString < y.toString) foreach printSourceAnnotations
    classOf[Foo12].getDeclaredConstructors foreach printParamSourceAnnotations
  }
}

object Test5 {
  import scala.beans.BeanProperty
  import java.lang.Integer

  class Count {
    // we use "Integer" instead of "Int" because of Java reflection
    @BeanProperty
    var count: Integer = 0

    private val getter =
      getClass().getMethod("getCount")
    private val setter =
      getClass().getMethod("setCount", classOf[Integer])

    def get = getter.invoke(this).asInstanceOf[Integer].intValue
    def set(n: Int) = setter.invoke(this, Integer.valueOf(n))
  }
  def run: Unit = {
    val count = new Count
    println(count.get)
    count.set(99)
    println(count.get)
  }
}

object Test6 {
  import scala.beans.BeanProperty
  import scala.beans.BooleanBeanProperty
  class C(@BeanProperty var text: String)
  class D(@BooleanBeanProperty var prop: Boolean) {
    @BeanProperty val m: Int = if (prop) 1 else 2
  }

  def run: Unit = {
    val c = new C("bob")
    c.setText("dylan")
    println(c.getText())
    val d = new D(true)
    d.setProp(false)
    if (!d.isProp()) {
      println(new D(false).getM())
    }
  }
}

// #3345
class A3345(@volatile private var i:Int)

object Test {
  def main(args: Array[String]): Unit = {
    Test2.run
    Test3.run     // requires the use of -target:jvm-1.5
    Test4.run
    Test5.run
    Test6.run
  }
}
