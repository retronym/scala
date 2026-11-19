//> using options -Werror

// as for a monomorphic method defined in Java, no parameter list is needed to override it
class S extends J {
  override def list[T]: java.util.List[T] = new java.util.ArrayList[T]
}
