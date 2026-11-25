package q

class S extends p.J {
  def viaThis = m() + f
  def static = p.J.sm() + p.J.N.nm()
  class Inner { def viaInner(s: S) = s.m() + s.f } // uses an accessor in S
}

// S$ is not a subclass of p.J, so the JVM does not let it access the protected instance members.
object S {
  def method(s: S) = s.m()
  def field(s: S) = s.f
  def assign(s: S) = { s.f = 5; 5 }
  def nested = p.J.N.nm() // ok, a protected class is public in bytecode
}
