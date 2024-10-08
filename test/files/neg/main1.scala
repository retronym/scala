//> using options -Xfatal-warnings
//
// negatives
package foo1 {
  object Foo {  // companion is trait
    def main(args: Array[String]): Unit = ()
  }
  trait Foo
}

package foo2 {
  object Foo {  // companion has its own main
    def main(args: Array[String]): Unit = ()
  }
  class Foo {
    def main(args: Array[String]): Unit = ()
  }
}

// these should all be made to work, but are negatives for now
// because forwarders need more work.

package foo3 {
  object Foo {  // Companion contains main, but not an interfering main.
    def main(args: Array[String]): Unit = ()
  }
  class Foo {
    def main(args: Int): Unit = ()
  }
}

package foo4 {
  object Foo extends Foo {  // Inherits main from the class
  }
  class Foo {
    def main(args: Array[String]): Unit = ()
  }
}

package foo5 {
  object Foo extends Foo {  // Overrides main from the class
    override def main(args: Array[String]): Unit = ()
  }
  class Foo {
    def main(args: Array[String]): Unit = ()
  }
}

// extended messaging

package p6 {
  object Main {
    def main(args: Array[Int]) = ()
  }
}

package p7 {
  trait Main
  object Main {
    def main(args: Array[Int]) = ()
  }
}

package p8 {
  trait Main
  object Main {
    def main(args: Array[Int]) = ()
    def main(args: Array[Double]) = ()
  }
}

package t7448 {
  object Main {
    def main(args: Array[String]) = ???
  }
}
