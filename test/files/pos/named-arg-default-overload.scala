object Test {
  def g(a: Any): String = "g(Any)"
  def g(a: Any, b: Int = 0): String = "g(Any, Int = 0)"

  g(1)
  // reported as ambiguous, although only the first alternative applies without a default argument
  g(a = 1)
}
