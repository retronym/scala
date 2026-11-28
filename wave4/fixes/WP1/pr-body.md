A guarded switch whose arms are boxed after erasure leaves an ill-typed tree, and `-Ycheck:all` reports it:

```scala
def f(s: Int): Any = s match { case 1 => 0.5; case 2 if s > 0 => 1.0; case _ => 2.0 }
```

```
Caught java.lang.AssertionError: assertion failed: Tree has erroneous type: default4()
Caught java.lang.AssertionError: assertion failed: Tree has erroneous type: scala.Double.box(2.0)
```

On 2.13.18 the checker then throws `NullPointerException`s and the compilation writes no class files. The bytecode, when written, is correct.

The switch translation collapses guarded cases and jumps from a failed guard to a `default` label wrapped around the last case. `wrapInDefaultLabelDef` gives the label the type of its body, so here `default4` is `()Double`. Erasure then boxes the case bodies of the `Any`-typed match: `box` pushes into the `LabelDef` and types the `LabelDef` and the jumps to it `Object`, but the label symbol keeps `()Double`. The checker retypes from the symbol and finds `Object` where `Double` is declared. Any phase or plugin reading the label's result type after erasure sees the wrong one. The same match with `String` arms, or with result type `Double`, involves no boxing and checks clean.

This commit gives the label the type of the match, as `matchEnd` already has, so erasure boxes the body inside the label instead of around it. A body of type `Nothing` (the synthetic `throw new MatchError`) keeps its own type: erasure must not try to box a `throw`, which is what scala/scala#10926 fixed (pos/t13060.scala).

The label has been typed from its body since at least 2.10; this tree shape first carries a default label in 2.13.11, the first release where the check fails (2.13.10 and 2.12.20 are clean). A variant remains: when the match itself has a primitive type but one arm erases to `Object` (`x match { case 1 => id(1.0); case 2 if e => 2.0; case _ => 3.0 }` with a generic `id`), erasure lubs the cases to `Object` and boxes the label body although the label is `()Double`. Fixing that needs erasure to keep label symbols in step with the boxing it pushes into `LabelDef`s, which is beyond this change.

Related fork pull requests for other `-Ycheck` reports: retronym/scala#419 (non-destructive `-Ycheck`), retronym/scala#460, retronym/scala#509.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WP1). Prepared with Claude.
