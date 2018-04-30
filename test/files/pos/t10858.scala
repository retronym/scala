import language.implicitConversions

class Test1 {
  implicit def foo(a: Int)(b: Int, c: Int): String = "" + a + b
  implicitly[Int => (Int, Int) => String]
}


class Test2 {
  implicit def foo(a: Int)(b: Int): String = "" + a + b
  implicitly[Int => (Int) => String]
}

class Test3 {
  implicit def foo(a: Int)(b: Int)(c: String, e: String): String = "" + a + b
  implicitly[Int => (Int) => (String, String) => String]
}
