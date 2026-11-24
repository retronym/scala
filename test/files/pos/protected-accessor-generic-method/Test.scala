//> using options -Ycheck:superaccessors

import j.JP

// The accessors for `g` have their own type parameter `U`, not that of `JP.g`,
// whose bound is seen from the host.
class C extends JP[CharSequence] {
  def clo = (() => g("a", "b"))()
  class I { def a = g("a", "b") }
}

class D[A] extends JP[A] {
  class I { def a(o: D[String]) = o.g("a", "b") }
}
