A match on an `if` whose condition is a compile-time constant crashes the back end when the surviving branch throws:

```scala
object Config { final val Enabled = false }
def describe = (if (Config.Enabled) level() else sys.error("disabled")) match {
  case 0 => "off"; case 1 => "on"; case _ => "other"
}
```

fails in `jvm` with `assertion failed: Bad superClass for class Int: <none>`, an error that names neither the match nor the flag and that appears or disappears when the flag is flipped. With an extractor pattern the same cause gives "error during expansion of this match (this is a scalac bug)", and a branch that is narrower but not `Nothing` gives the same expansion error (`(if (false) 1 else "s") match { case "s" => ... case 1 => ... }`, or an else-if chain on flags, `(if (Cfg.A) 1 else if (Cfg.B) "s" else 2.0) match { ... }`) or a false "unreachable code" warning (`(if (false) A() else B()) match { case A() => ...; case B() => ... }` on a sealed hierarchy, or `(if (true) null else s) match { case "a" => ...; case null => ...; case _ => ... }`). Outside a match, a `Nothing` branch gives code that fails verification (`(if (false) "s" else ???).length`, and `(if (false) new V(1) else ???) match { case v => v.x }` for a value class `V`), and a `null` branch of an array-typed `if` crashes the back end (`(if (true) null else a).length` gives `scala.Array.length : Null(null)`). 2.12 compiles all of these.

`RefChecks.transformIf` replaces an `if` with a foldable constant condition by the chosen branch as is. The `if` had the type its consumers were typed against (`Int`, the lub with `Nothing`); the branch has its own, narrower type. Since scala/scala#7260 the pattern matcher runs after refchecks and takes the scrutinee type from the selector tree, so it declares `val x1: Nothing` and builds the literal test `3 == x1`, for which overload resolution on `Int.==` fails without a reported error; the tree with an error symbol reaches the back end. A selection on a `Nothing` operand reaches the back end as a call on `Nothing$`, which the verifier rejects, and the back end picks an array instruction from the receiver's type, which for a `null` branch is `Null`.

The fix keeps the narrower type away from the consumers that depend on it, in two places. The `Match` case of the RefChecks transformer compares the transformed selector's type with the selector's original type, after widening literal and singleton types, and if they differ ascribes the original type (`Typed(selector1, TypeTree(originalType))`). This is independent of how deep the reduced `if` sits in the selector (an else-if chain, a constant `if` in a branch of another) and of what narrowed it. The `if` case ascribes the `if`'s type to a surviving branch of type `Nothing`, and to a `null` branch of an array-typed `if`, which is what selections, unboxing and array operations on the branch need. Elsewhere the branch is used as before. Writing the `Nothing`-typed scrutinee directly is rejected by typer, so this reduction was the only way to reach the pattern matcher with it.

The `if` case also sends the surviving branch through the transform itself, not only its children, so the branch's own node is handled as in any other position: a match that is itself the surviving branch is seen as a match, and a constant `if` inside a reduced branch is reduced too.

What changes for code that compiled before:

- A match whose selector is narrowed by the reduction is translated against the selector's declared type. Where that already compiled correctly, the generated test can differ: `(if (true) s else 1) match { case "a" => ... }` with `s: String` no longer uses a string switch but `"a".equals(x)`, as `(x: Any) match` does. Results are the same.
- `(if (true) null else a) == b` on an array-typed `if` compiles to a reference comparison of `null` with `b` instead of a null test of `b`; same result.
- A constant `if` nested in a reduced branch is reduced in refchecks rather than compiled as a conditional jump on a constant: `if (F) 1 else if (T) 2 else 3` compiles to `2`. A case-class `B()` branch compiles to `new B` rather than `B.apply()`, as `B()` written alone does.
- A reduced branch now gets the RefChecks warnings it skipped before, the same as the code gives outside a constant `if`: `if (C.T) old else 0` warns that `old` is deprecated, and comparing `Int` and `String` with `==`, a pure expression in a block branch and pattern shadowing are reported there. Builds with `-Werror` can see these new warnings. A forward reference in a bare identifier branch is now a proper error where the back end crashed with "key not found".

Ordinary constant `if`s outside match selectors keep the bytecode of their surviving branch: `(if (true) null else s) == t`, `(if (true) "x" else t) == t`, a string switch on `(if (true) "a" else s)`, a statement `if (true) 1 else "s"`, a `Nothing` branch in return position and a by-name argument `if (true) 1 else "s"` compile as before. A match on a reduced `if` whose type is the selector's (other than by a literal type) is unchanged.

Long-standing, a regression from 2.12 in 2.13.0 (scala/scala#7260 moved the pattern matcher behind the reduction). scala/bug#11200 has the same assertion text from a different cause. No ticket for this cause.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GF1). Prepared with Claude.
