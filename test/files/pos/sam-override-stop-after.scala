//> using options -Ystop-after:uncurry

// uncurry asks erasure whether LambdaMetaFactory can bridge between a SAM and the
// method it overrides. That check travels to the phase after erasure, which used
// to throw an NPE when erasure is not part of the run.
trait P { def apply(x: Int): Any }
trait F extends P { def apply(x: Int): String }
object Test { val f: F = x => "" }
