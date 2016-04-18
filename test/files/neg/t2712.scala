package test

object Test {
  scala.util.Random.shuffle("abcd".map(x => (x, x.toInt)).toMap)
}
