// This test fails: it records scala/bug#4744. The same files compile with -Ybreak-cycles
// (pos/t4744); without it, scalac reports an illegal cyclic reference for the Java class.
class Bar { val quux = new Foo[java.lang.Integer]() }
