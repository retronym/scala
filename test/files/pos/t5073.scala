
trait Test {

  trait T[A]

  class C {
    def -:(x: AnyRef): T[x.type] = ???
    def +:(x: AnyRef)(i: Int): T[x.type] = ???
  }

  val c = new C
  val x: AnyRef = ???

  def ok: T[x.type] = c.-:(x)
  def no: T[x.type] = x -: c

  def ok2: (Int => T[x.type]) = c.+:(x) _
  def no2: (Int => T[x.type]) = (x +: c) _
  def no3: (Int => T[x.type]) = (x +: c)(_)
}
