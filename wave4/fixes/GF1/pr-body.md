A match on an `if` whose condition is a compile-time constant crashes the back end when the surviving branch throws:

```scala
object Config { final val Enabled = false }
def describe = (if (Config.Enabled) level() else sys.error("disabled")) match {
  case 0 => "off"; case 1 => "on"; case _ => "other"
}
```

fails in `jvm` with `assertion failed: Bad superClass for class Int: <none>`, an error that names neither the match nor the flag and that appears or disappears when the flag is flipped. With an extractor pattern the same cause gives "error during expansion of this match (this is a scalac bug)", and a branch that is narrower but not `Nothing` gives the same expansion error (`(if (false) 1 else "s") match { case "s" => ... case 1 => ... }`) or a false "unreachable code" warning (`(if (false) A() else B()) match { case A() => ...; case B() => ... }` on a sealed hierarchy, or `(if (true) null else s) match { case "a" => ...; case null => ...; case _ => ... }`). Outside a match, a `Nothing` branch gives code that fails verification: `(if (false) "s" else ???).length`, and `(if (false) new V(1) else ???) match { case v => v.x }` for a value class `V`. 2.12 compiles all of these.

`RefChecks.transformIf` replaces an `if` with a foldable constant condition by the chosen branch as is. The `if` had the type its cases were typed against (`Int`, the lub with `Nothing`); the branch has its own, narrower type. Since scala/scala#7260 the pattern matcher runs after refchecks and takes the scrutinee type from the tree, so it declares `val x1: Nothing` and builds the literal test `3 == x1`, for which overload resolution on `Int.==` fails without a reported error; the tree with an error symbol reaches the back end. A selection on a `Nothing` operand reaches the back end as a call on `Nothing$`, which the verifier rejects.

The fix wraps the branch in a `Typed` with the `if`'s type in the two places where the narrower type is seen: when the branch has type `Nothing`, and when the `if` is the scrutinee of a match and the branch's type differs from the `if`'s by more than a literal type. Elsewhere the branch is used as before, so code that compiled correctly before gets the same bytecode: wrapping every reduced `if` would, for example, turn `(if (true) "x" else t) == t` into a call of `Objects.equals`, add a null check to a string switch on `(if (true) "a" else s)`, and lose the specialized lambda for a by-name argument `if (true) 1 else "s"`. Writing the `Nothing`-typed scrutinee directly is rejected by typer, so this reduction was the only way to reach the pattern matcher with it.

Long-standing, a regression from 2.12 in 2.13.0 (scala/scala#7260 moved the pattern matcher behind the reduction). scala/bug#11200 has the same assertion text from a different cause. No ticket for this cause.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GF1). Prepared with Claude.
