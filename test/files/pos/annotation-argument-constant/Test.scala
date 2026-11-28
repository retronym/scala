// Every form of constant expression an annotation argument may take (SLS 6.24, 11).
object K {
  final val One = 1
  final val T = true
  final val L = 2L
  final val D = 1.5
  final val Hello = "hello"
}
object Other { final val Two = K.One + 1 }

class C(i: Int) extends scala.annotation.ConstantAnnotation
class CD(s: String = "dflt", i: Int = 1) extends scala.annotation.ConstantAnnotation

object Test {
  final val Local = 4

  // literals
  @Ann(z = false, b = 1, sh = 2, c = 'x', i = 3, l = 4L, f = 1.5f, d = 2.5, s = "a") class Lit1
  @Ann(i = 0x10, l = -1L, d = 1e3) class Lit2

  // unary operators on literals and on constants
  @Ann(z = !true) class Un1
  @Ann(z = !false) class Un2
  @Ann(i = ~1) class Un3
  @Ann(i = -(1)) class Un4
  @Ann(i = +(1)) class Un5
  @Ann(d = -(1.5), l = ~(2L)) class Un6
  @Ann(i = -K.One, z = !K.T, l = ~JK.L) class Un7
  @Ann(i = -(-(1))) class Un8

  // binary operators on literals and on constants
  @Ann(i = 1 + 2, l = 3L * 4, d = 1.0 / 3, z = 1 < 2) class Bin1
  @Ann(i = 1 << 3, z = true && false, c = 'a') class Bin2
  @Ann(i = K.One + 2, l = K.L * JK.L, d = K.D * 2, z = K.T ^ JK.Z) class Bin3
  @Ann(i = (1 + K.One) * Other.Two - JK.I % 2) class Bin4
  @Ann(i = 'a' + 1, z = 'a' == 97) class Bin5
  @Ann(i = -1 + ~2, z = !(1 == 2)) class Bin6

  // string concatenation
  @Ann(s = "a" + "b") class Str1
  @Ann(s = K.Hello + " world" + JK.S + pk.PS) class Str2

  // conversions
  @Ann(i = K.One.toInt, l = JK.I.toLong) class Conv1

  // final vals in the same object, another object, a package object, Java static finals
  @Ann(i = Local, l = Test.Local) class Ref1
  @Ann(i = Other.Two, l = JK.L, z = JK.Z, s = JK.S) class Ref2
  @Ann(i = pk.PK, s = pk.PS) class Ref3

  // classOf, enum constants, arrays, nested annotations
  @Ann(cls = classOf[String], e = Color.Green) class Cls1
  @Ann(cls = classOf[Array[Int]]) class Cls2
  @Ann(is = Array(1, K.One + 1, -(3)), ss = Array("a", K.Hello + "!"), es = Array(Color.Red, Color.Green)) class Arr1
  @Ann(is = Array()) class Arr2
  @Ann(n = new Nested(3), ns = Array(new Nested(1), new Nested(value = K.One), new Nested)) class Nest1

  // named and default arguments
  @Ann() class Def1
  @Ann(s = "x") class Def2
  @CD() class Def3
  @CD(i = 3) class Def4
  @CD(s = K.Hello, i = -(2)) class Def5

  // parenthesised and typed forms
  @Ann(i = (1), s = ("a")) class Par1
  @Ann(i = ((K.One) + (2)), z = (!(true))) class Par2
  @Ann(i = (1: Int), s = (K.Hello: String), l = (JK.L: Long)) class Typed1

  // Scala constant annotations
  @C(K.One * 2) class SC1
  @C(-(1)) class SC2
  @C(~JK.I) class SC3
}
