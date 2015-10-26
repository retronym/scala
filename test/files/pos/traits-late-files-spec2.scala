class Parent[@specialized(Int) T]

class Val[@specialized(Int) T](t: T, expectedXSuper: String) {
  val check: T = {
    class X extends Parent[T]()
    new X // spurious type error during specialization under adriaanm/traits-late-fields @ 6294731c
          // "class X cannot be instantiated because it does not conform to its self-type Val$mcI$sp.this.X"
    t
  }
}
