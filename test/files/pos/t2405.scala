object A { implicit val x: Int = 1 }

// Problem as stated in the ticket.
object Test1 {
	import A.{x => y}
	implicitly[Int]
}

// Testing for the absence of shadowing #1.
object Test2 {
	import A.{x => y}
	val x = 2
	implicitly[Int]
}

// Testing for the absence of shadowing #2.
object Test3 {
	{
		import A.{x => y}
		def x: Int = 0
		implicitly[Int]
	}
}

// One implicit imported under two names is one candidate.
object Test4 {
  import A.x
  import A.{x => y}
  implicitly[Int]
}

object Test5 {
  import A.{x, x => y}
  implicitly[Int]
}

object Test6 {
  object P { val a: A.type = A }
  import A.x
  def f = { import P.a.{x => z}; implicitly[Int] }
}
