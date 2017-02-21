 class C[A] { final def foo(a: A) = "c" }
 class D extends C[String] { def foo(implicit s: String) = "d" }
 // was a verify error on classloading D