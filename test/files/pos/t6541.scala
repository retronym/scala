trait B[T]

// The compiler infered a non SIP-18 compliant return type
// for C.unapply: Option[(Any, B[_$1], Int)] forSome { type _$1 }
case class C(a: Any, b: B[_], c: Int) {
}

object Test {
	val c = C("", new B[Any] {}, 0)
	val Some((_, b, _)) = C.unapply(c)
	b match {
		case _: B[_] =>
	}
}
