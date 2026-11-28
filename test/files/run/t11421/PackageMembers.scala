// Members of packages `q` and `r` that share a name with the implicits imported in Test.scala. They are
// defined in another file, so the imports there take precedence over them.
package object q {
  def z: Int = 0
}
package r {
  object z
}
