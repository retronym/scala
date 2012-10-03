object OL {
	def f(xs: List[Int], ys: AnyRef*) = () ; def f(xs: AnyRef*) = ()

	f(List(): _*)
	f(Nil)
	f(Array(): _*)
}
