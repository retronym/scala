// test mixin of getters / setters, and implementing abstract
// methods using @BeanProperty
class C extends T with BeanF {
  def foo() {
    setF("doch!")
    setG(true)
    this.getF()
  }
}

trait T {
  @scala.beans.BeanProperty var f = "nei"
  @scala.beans.BooleanBeanProperty var g = false
}

trait BeanF {
  def getF(): String
  def setF(n: String): Unit

  def isG(): Boolean
  def setG(nb: Boolean): Unit
}

/*
    @scala.beans.BeanProperty private[this] var f: String = "nei";
    <accessor> def f: String = T.this.f;
    <accessor> def f_=(x$1: String): Unit = T.this.f = x$1;
    def setF(x$1: String): Unit = T.this.f = x$1;
    @scala.beans.BooleanBeanProperty private[this] var g: Boolean = false;
    <accessor> def g: Boolean = T.this.g;
    <accessor> def g_=(x$1: Boolean): Unit = T.this.g = x$1;
    def setG(x$1: Boolean): Unit = T.this.g = x$1;
    def getF(): String = T.this.f;
    def isG(): Boolean = T.this.g
*/


trait T { final val bla: Int = 123 }
class C extends T // bla should be final in C



trait T { final val C = "S" }
// there should be a C method in `T$class`!
class C extends T { println(C) }

trait T {
  type Res
  val x: Res = ???
}

abstract class MiddleMan extends T

class C extends MiddleMan with T

//

abstract class Context extends Aliases with AliasesOverrides

trait Aliases {
  val TypeTag = "universe.TypeTag"
}

trait AliasesOverrides extends Aliases { // or self: Aliases =>
  override val TypeTag = "the real universe.TypeTag"
}

// TODO: check that this prints the right thing....

////////////

trait B { val y = "a" }
trait T extends B {
  val x: y.type = y
}

/////////////
trait Foo { self: Meh =>
  def bar(x: String) = x == "a"
  private final val meh = bar("a")
}

abstract class Meh extends Foo
/////////////

trait SymbolTable {
  def currentRunId: Int
}

trait ReflectSetup extends SymbolTable {
  override val currentRunId = 1
}

class G extends SymbolTable with ReflectSetup

trait OneVal { val x: Int = 123 }
// class Conflicting extends OneVal { def x: Int = 1 } // error expected TODO: right message
class OverridingVal extends OneVal { override val x: Int = ??? }


trait VolatileAbort {
  @volatile private var abortflag = false
}

class DefaultSignalling extends VolatileAbort

trait V {
  // ok
  // error: java.lang.IllegalArgumentException: Could not find proxy for val f: Function1 in List(value f, value v, trait V, package <empty>, package <root>) (currentOwner= value <local V$class> )
  val v = { val f = (x: Int) => x + 1; f(2) }

  // ok
  // assertion failed:
  //   Trying to access the this of another class: tree.symbol = trait V, class symbol = object V$class compilation unit: fields.scala
  val developmentVersion =
    for {
      v <- scalaPropOrNone("maven.version.number")
      if v endsWith "-SNAPSHOT"
      ov <- scalaPropOrNone("version.number")
    } yield ov

  def scalaPropOrNone(name: String): Option[String] = ???
}

object O extends V


// done
// test/files/trait-defaults/fields.scala:24: error: double definition:
// def signalDelegate_=(x$1: Signalling): Unit at line 24 and
// def signalDelegate_=(x$1: Signalling): Unit at line 24
// have same type
// class SUB extends IterableSplitter
//       ^
// one error found

trait Signalling

trait DelegatedSignalling extends Signalling {
  var signalDelegate: Signalling
}

trait IterableSplitter extends DelegatedSignalling {
  var signalDelegate: Signalling = ???
}

class SUB extends IterableSplitter



// package <empty> {
//   abstract trait OneAbstractVar extends Object {
//     <accessor> def OneAbstractVar$_setter_$x_=(x$1: Int): Unit;
//     <stable> <accessor> def x(): Int
//   };
//   class ConcVar extends Object with OneAbstractVar {
//     <accessor> def OneAbstractVar$_setter_$x_=(x$1: Int): Unit = ();
//     private[this] val x: Int = _;
//     override <stable> <accessor> def x(): Int = ConcVar.this.x;
//     def <init>(): ConcVar = {
//       ConcVar.super.<init>();
//       OneAbstractVar$class./*OneAbstractVar$class*/$init$(ConcVar.this);
//       ConcVar.this.x = scala.this.Predef.???();
//       ()
//     }
//   };
//   abstract trait OneAbstractVar$class extends  {
//     def /*OneAbstractVar$class*/$init$($this: OneAbstractVar): Unit = {
//       $this.OneAbstractVar$_setter_$x_=(123);
//       ()
//     }
//   }
// }

class Nest { val x = println(1)}

package scala

trait OneConcreteVal[T] {
  @deprecatedOverriding val x = 1 // : T = ???
  @volatile  var vy = "a"
  println(x)
  def foo = x
}


trait OneOtherConcreteVal[T] {
  var y: T = ???
}

class C extends OneConcreteVal[Int] with OneOtherConcreteVal[String]

object T extends App {
  val c = new C
  println(c.x)
  println(c.y)
}
/*
old decls for trait trait OneOtherConcreteVal: Scope{
  def y(): Object;
  def y_=(x$1: Object): Unit
}
new decls for trait trait OneOtherConcreteVal: Scope{
  def y(): Object;
  def y_=(x$1: Object): Unit;
  def $init$(): Unit
}
old decls for trait trait OneConcreteVal: Scope{
  val x(): Int
}
new decls for trait trait OneConcreteVal: Scope{
  val x(): Int;
  def $init$(): Unit;
  private[this] def x_=(x$1: Int): Unit
}
old decls for class C: Scope{
  def <init>(): C
}
mixing in List(method y, method y_=, value x, method x_=) from List(trait OneOtherConcreteVal, trait OneConcreteVal)
()String
()Object
()String
()Object
(x$1: String)Unit
(x$1: Object)Unit
()Int
()Int
()Int
()Int
(x$1: Int)Unit
(x$1: Int)Unit
new decls for class C: Scope{
  def <init>(): C;
  def y(): Object;
  private[this] var y: Object;
  def y_=(x$1: Object): Unit;
  val x(): Int;
  private[this] val x: Int;
  private[this] def x_=(x$1: Int): Unit
}
*/


class Meh {
  final val x = 1
  def foo = x
}
class CE extends Empty

trait T {
  val abs: String
  protected val protabs: String
  val pub = "public"
  protected val prot = "protected"
  private val privvy = "private"
  private[this] val privateThis = "private[this]"
  // TODO:
  // final val const = "const"

  trait Nested { println(abs + privateThis) }

  object NO {
    println(abs)
    println(pub)
    println(prot)
    println(protabs)
    println(privvy)
    println(privateThis)
  }

  trait NT {
    println(abs)
    println(pub)
    println(prot)
    println(protabs)
    println(privvy)
    println(privateThis)
  }

  class NC {
    println(abs)
    println(pub)
    println(prot)
    println(protabs)
    println(privvy)
    println(privateThis)
  }
}

class C extends AnyRef with T {
  println("x")
  val abs = "abstract"
  println("y")
  val protabs = "abstract protected"
  final val const = "const"
  println("z")
}

object Test extends C {
  def main(args: Array[String]): Unit = {
  NO
  new NT{}
  new NC
}}