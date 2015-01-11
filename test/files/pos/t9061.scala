class Demo {
  def byName(a: => Any) = ???
  byName({ object Blah; Blah }).toString _
}
