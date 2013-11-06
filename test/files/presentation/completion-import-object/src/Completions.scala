package test

import scala.Predef.DummyImplicit // turn off most of Predef for a cleaner .check file.

class C {
  val ccc : Int = 0
}

object O extends C {
  val ooo : Int = 0
}

class Foo {
  {
    val o = O
    import o._
    /*_*/
  }
  {
    import O._
    /*_*/
  }
  {
    val c = new C
    import c._
    /*_*/
  }
}
