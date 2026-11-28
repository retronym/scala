//> using options -Werror
import annotation.nowarn

class C {
  def id[A](a: A): A = a

  // The `return` is local in the source, and the handler does not intercept it.
  @nowarn("msg=catches all Throwables")
  def lifted(b: Boolean): Int = id(try { if (b) return 1; 2 } catch { case _ => 3 })

  // The `return` is in a closure, and the handler intercepts it.
  @nowarn("msg=catches all Throwables")
  def closure(l: List[Int]): Int = id(try { l.foreach(_ => return 1); 2 } catch { case _ => 3 })
}
