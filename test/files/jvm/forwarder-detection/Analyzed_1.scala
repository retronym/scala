trait T {
  def t = "t"
}
trait U {
  def u = "u"
}
class Forwarder extends T
object Forwarder extends U

class ValueClass(val a: Any) extends AnyVal {
  def vc = "vc"
}

class ImplicitClass {
  implicit class C(s: String)
  private implicit class D (s: String)
}
object ImplicitClass {
  // TODO Not detected as an implicit class factory method as the
  //      constructor call is erased
  //
  // public java.lang.String E(java.lang.String);
  //  Code:
  //   Stack=1, Locals=2, Args_size=2
  //   0:	aload_1
  //   1:	areturn
  //  LocalVariableTable:
  //   Start  Length  Slot  Name   Signature
  //   0      2      0    this       LImplicitClass$;
  //   0      2      1    s       Ljava/lang/String;
  implicit class E (val s: String) extends AnyVal
}
