class Parent

class Val[@specialized(Int) T](t: T, expectedXSuper: String) {
  class X extends Parent()

  val check: T = {new X; t} // compiler crash under adriaanm/traits-late-fields @ 6294731c
                            // "value <none> is not a member of Val[T]"
}
