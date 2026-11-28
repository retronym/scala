//> using options -Ycheck:erasure,posterasure

// Boxing `$this` in `hashCode$extension` passes the class's `Unit` value boxed to its constructor.
class VU(val u: Unit) extends AnyVal
