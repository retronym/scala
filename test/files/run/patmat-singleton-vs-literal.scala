//> using options -Werror

// `_: p.type` compares with `eq`, a literal pattern or literal type pattern with `==`.
// The tests are different even when the type of `p` is the literal type.
object Test {
  val s: "x" = "x"
  val t: s.type = s

  def litPat(a: Any)  = a match { case _: s.type => "eq"; case "x" => "lit"; case _ => "no" }
  def litTp(a: Any)   = a match { case _: s.type => "eq"; case _: "x" => "lit"; case _ => "no" }
  def stable(a: Any)  = a match { case _: s.type => "eq"; case `s` => "lit"; case _ => "no" }
  def alias(a: Any)   = a match { case _: t.type => "eq"; case "x" => "lit"; case _ => "no" }
  def guarded(a: Any) = a match { case _: s.type if a.hashCode == 0 => "eq"; case "x" => "lit"; case _ => "no" }
  def nested(a: Any)  = a match { case Some(_: s.type) => "eq"; case Some("x") => "lit"; case _ => "no" }
  def twice(a: Any)   = a match { case (_: s.type, 1) => "eq1"; case (_: s.type, 2) => "eq2"; case ("x", _) => "lit"; case _ => "no" }

  // Both cases test `1 == a`, and the second reuses the result of the first, stored along with the value cast to `one.type`.
  val one: 1 = 1
  def reused(a: Any, b: Boolean) = a match { case _: one.type if b => "path"; case _: 1 => "lit"; case _ => "no" }

  def main(args: Array[String]): Unit = {
    assert(reused(1, true) == "path")
    assert(reused(1, false) == "lit")
    assert(reused(2, true) == "no")

    val fs = List[(String, Any => String)]("litPat" -> litPat, "litTp" -> litTp, "stable" -> stable, "alias" -> alias, "guarded" -> guarded)
    val x = new String("x")
    assert(x ne s)
    for ((name, f) <- fs) {
      assert(f(x) == "lit", s"$name: ${f(x)}")
      assert(f("y") == "no", name)
      if (name != "guarded") assert(f(s) == "eq", name)
    }
    assert(nested(Some(x)) == "lit", nested(Some(x)))
    assert(nested(Some(s)) == "eq")
    assert(twice((s, 1)) == "eq1")
    assert(twice((s, 2)) == "eq2")
    assert(twice((x, 2)) == "lit", twice((x, 2)))
    assert(twice((s, 3)) == "lit")
  }
}
