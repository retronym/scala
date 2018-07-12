

trait Matcher[-T]{
  def and[U, TC1[_]](other: MatcherFactory1X[U, TC1]): MatcherFactory1X[T with U, TC1]
	def be = {
	  val temp: MatcherFactory1X[Any,SortableX] = null
	  and(temp)
	}
  val temp = (null: (this.type with SortableX[Any])).be
}
trait SortableX[-A]
trait MatcherFactory1X[-SC, TC1[_]]

