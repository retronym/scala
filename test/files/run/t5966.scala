object o { def apply(i: AnyRef*)(j: String) = i }

object Test {
	def main(args: Array[String]) {
		println("(o()_)(\"\") = " + (o()_)(""))
	}
}
