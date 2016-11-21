package foo

import scala.annotation.varargs

trait AbstractProps {
  @varargs
  def create(x: String, y: Int*): AbstractProps = null

  @varargs
  private def create2(x: String, y: Int*): AbstractProps = null
}
