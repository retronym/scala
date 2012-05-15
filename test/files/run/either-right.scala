object Test extends App {
  val l: Either[String, Int] = Left("flower")
  val r: Either[String, Int] = Right(12)

  // Use of projections, as before
  assert(l.left.map(_.size) ==  Left(6))
  assert(r.left.map(_.size) == Right(12))
  assert(l.right.map(_.toDouble) == Left("flower"))
  assert(r.right.map(_.toDouble) == Right(12.0))

  assert(l.map(_.toDouble) == Left("flower"))
  assert(r.map(_.toDouble) == Right(12.0))

  val r12 = for {
    r1 <- r
    r2 <- r
    // filter/withFilter not implemented intentionally, so no `if x` here.
  } yield (r1, r2)
  assert(r12 == Right((12, 12)))

  val l12 = for {
    l1 <- l
    l2 <- l
    // filter/withFilter not implemented intentionally, so no `if x` here.
  } yield (l1, l2)
  assert(l12 == l)

  assert(r.exists(_ == 12) == true)
  assert(l.exists(_ => true) == false)
}
