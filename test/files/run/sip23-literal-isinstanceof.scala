// SLS 3.2.1: `e.isInstanceOf[lit]` and `e match { case _: lit => }` are both determined by `e == lit`.
object Test extends App {
  def check(x: Any, isI: Boolean, matches: Boolean, eqeq: Boolean): Unit = {
    assert(isI == eqeq, s"isInstanceOf on $x (${x.getClass}): $isI, expected $eqeq")
    assert(matches == eqeq, s"pattern match on $x (${x.getClass}): $matches, expected $eqeq")
  }

  val long1: Any = 1L
  check(long1, long1.isInstanceOf[1], long1 match { case _: 1 => true case _ => false }, long1 == 1)
  check(long1, long1.isInstanceOf[1L], long1 match { case _: 1L => true case _ => false }, long1 == 1L)
  check(long1, long1.isInstanceOf[2], long1 match { case _: 2 => true case _ => false }, long1 == 2)

  val char97: Any = 'a'
  check(char97, char97.isInstanceOf[97], char97 match { case _: 97 => true case _ => false }, char97 == 97)
  check(char97, char97.isInstanceOf['a'], char97 match { case _: 'a' => true case _ => false }, char97 == 'a')

  val int97: Any = 97
  check(int97, int97.isInstanceOf['a'], int97 match { case _: 'a' => true case _ => false }, int97 == 'a')

  val negZero: Any = -0.0
  check(negZero, negZero.isInstanceOf[0.0], negZero match { case _: 0.0 => true case _ => false }, negZero == 0.0)

  val float1: Any = 1.0f
  check(float1, float1.isInstanceOf[1.0], float1 match { case _: 1.0 => true case _ => false }, float1 == 1.0)

  val str: Any = new String("ab")
  check(str, str.isInstanceOf["ab"], str match { case _: "ab" => true case _ => false }, str == "ab")
  check(str, str.isInstanceOf["ba"], str match { case _: "ba" => true case _ => false }, str == "ba")

  val nul: Any = null
  check(nul, nul.isInstanceOf["ab"], nul match { case _: "ab" => true case _ => false }, nul == "ab")
  check(nul, nul.isInstanceOf[1], nul match { case _: 1 => true case _ => false }, nul == 1)

  // The pattern matcher also uses `==` for singleton types of value class type.
  val one: Int = 1
  check(long1, long1.isInstanceOf[one.type], long1 match { case _: one.type => true case _ => false }, long1 == one)

  // Stable references are still compared by reference.
  val s1 = new String("ab")
  val s2 = new String("ab")
  val any2: Any = s2
  assert(!any2.isInstanceOf[s1.type])
  assert(any2.isInstanceOf[s2.type])
}
