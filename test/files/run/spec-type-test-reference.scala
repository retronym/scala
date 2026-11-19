//> using options -Wconf:cat=unchecked:s
// A type test against a reference type in a specialized method must compile, and the specialized
// variant must give the same answer as the generic method, which tests the boxed value.

object Test {
  def isString[@specialized(Int) T](t: T): Boolean = t.isInstanceOf[String]
  def isInteger[@specialized(Int) T](t: T): Boolean = t.isInstanceOf[Integer]
  def isAny[@specialized(Int) T](t: T): Boolean = t.isInstanceOf[Any]
  def isAnyRef[@specialized(Int) T](t: T): Boolean = t.isInstanceOf[AnyRef]
  def isComparable[@specialized(Int) T](t: T): Boolean = t.isInstanceOf[Comparable[_]]
  def isComparableString[@specialized(Int) T](t: T): Boolean = t.isInstanceOf[Comparable[String]]
  def isSerializable[@specialized(Int) T](t: T): Boolean = t.isInstanceOf[java.io.Serializable]
  def isNumber[@specialized(Int) T](t: T): Boolean = t.isInstanceOf[Number]
  def isLong[@specialized(Int) T](t: T): Boolean = t.isInstanceOf[java.lang.Long]
  def isInt[@specialized(Int) T](t: T): Boolean = t.isInstanceOf[Int]
  def isDouble[@specialized(Int, Double) T](t: T): Boolean = t.isInstanceOf[Double]
  def isList[@specialized(Int) T](t: T): Boolean = t.isInstanceOf[List[_]]
  def unit[@specialized(Unit) T](t: T): List[Boolean] =
    List(t.isInstanceOf[String], t.isInstanceOf[scala.runtime.BoxedUnit], t.isInstanceOf[java.io.Serializable], t.isInstanceOf[Unit])
  def isCharSeqSer[@specialized(Int) T](t: T): Boolean = t.isInstanceOf[CharSequence with java.io.Serializable]

  def matching[@specialized(Int) T](t: T): String = t match {
    case s: String         => "String " + s.length
    case i: Int            => "Int " + (i + 1)
    case n: Number         => "Number"
    case c: Comparable[_]  => "Comparable"
    case _                 => "other"
  }
  def matchingInteger[@specialized(Int) T](t: T): String = t match {
    case i: Integer => "Integer " + i
    case _          => "other"
  }

  var effects = 0
  def effect[@specialized(Int) T](t: T): T = { effects += 1; t }
  def effectful[@specialized(Int) T](t: T): Boolean = effect(t).isInstanceOf[String]

  def all[T](t: T): List[Any] = List(
    isString(t), isInteger(t), isAny(t), isAnyRef(t), isComparable(t), isComparableString(t),
    isSerializable(t), isNumber(t), isLong(t), isInt(t), isDouble(t), isList(t), isCharSeqSer(t),
    matching(t), matchingInteger(t))

  def main(args: Array[String]): Unit = {
    println(all(1))
    println(all("s"))
    println(all(1.5))
    println(List(isString(1), isInteger(1), isAny(1), isAnyRef(1), isComparable(1), isComparableString(1),
      isSerializable(1), isNumber(1), isLong(1), isInt(1), isDouble(1), isDouble(1.5), isList(1), isCharSeqSer(1),
      matching(1), matchingInteger(1)))
    println((effectful(1), effects))
    println(unit(()))
  }
}
