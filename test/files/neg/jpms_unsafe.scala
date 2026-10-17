//> using options -Yjpms --module-path .
//> using jvm 11+

class C {
  class D
  new D

  jdk.internal.misc.Unsafe.getUnsafe
}
