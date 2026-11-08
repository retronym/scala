class Msgs2 {
  // the same clash on another thread, to overlap with Msgs
  val longs: List[java.lang.Long] = List[Long](1L)
  val same: p.Same = new q.Same
  def after: Long = 1L
}
