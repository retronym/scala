// The restriction is reported once for an assignment, not for the assignment and its left-hand side.
trait S extends j.J {
  def set(): Unit = { f = 2 }
}
