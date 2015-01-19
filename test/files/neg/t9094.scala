package object p {
  def free[A] = free.Free
}
package p.free {
  object Free
}
