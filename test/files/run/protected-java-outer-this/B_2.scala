import j.JP_1

class B extends JP_1[String] with Cloneable {
  // `K` is a subclass of `JP_1`, but the receiver `B.this` is not a `K`, so the JVM
  // rejects a direct access from `K`. An accessor in `B` is needed.
  class K extends JP_1[String] {
    def k = B.this.m()
    def k3 = B.this.f
    def k4 = { B.this.f = 7; B.this.f }
    def own = m() + this.f
    // the receiver is a `K`: accessible directly from `K`
    def param(o: K) = { o.f = 3; o.m() + o.f }
  }
  class K2 extends B { def k = B.this.m() + B.this.f }
  // every class is a subclass of `Object`, so this is the same shape for `Object.clone`
  def cl = new Runnable { def run() = println(B.this.clone().getClass) }
}
