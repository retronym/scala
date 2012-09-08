object X {
	def a = 0
	def b = 1
}

object Test {
	// expect an error at `c`
	import X.{a, b, c}

	// no errors expected below
	a.toInt
	b.toInt
}
