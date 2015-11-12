// TODO: bytecode test

// bytecode should reflect volatile annotation
trait VolatileAbort {
  @volatile private var abortflag = false
}
class DefaultSignalling extends VolatileAbort
