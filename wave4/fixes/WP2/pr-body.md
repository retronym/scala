After extmethods, a structural call in a method of a generic value class selects a member of a refinement that no longer appears in the tree's types:

```scala
class Box[A](val v: A) { def get(n: Int): A = v }
class V0[T](val u: T) extends AnyVal {
  def m: List[T] = { val s: { def get(n: Int): List[T] } = new Box(List(u)); s.get(1) }
}
```

`-Ycheck:pickler` (the check of the tree after extmethods) reports `Tree has erroneous type: s.get(1)`, and on 2.13.18 then throws a `NullPointerException` and writes no class files. The same method in an ordinary generic class checks clean.

ExtensionMethods moves the body of `m` into `m$extension[T]($this: V0[T])`, substituting the method's own `T` for the class's. Substitution maps the types in the body, which rebuilds the refinement `AnyRef{def get(n: Int): List[T]}` with a new member `get` typed with the new `T`, but the `Select` `s.get` keeps its symbol: the member of the old refinement, typed with the class's `T`. The selected symbol is then not a member of its qualifier's type. Erasure gives both type parameters the same erasure, so the bytecode is right; a phase or plugin running between extmethods and erasure that reads the selected symbol's type sees the class's type parameter.

This commit rebinds, after the substitution, each `Select` of a refinement member to the member of the same name and matching type in the qualifier's (new) refinement. A refinement the substitution did not change yields the same symbol. The new refinement class is still owned by the value class, so `-Ycheck` still prints an "Out of scope symbol reference" warning for it inside the extension method; giving it the extension method as owner would mean cloning the refinement class during substitution, which I have left alone.

Long-standing: 2.12.20 reports the same. The test checks only the phases before erasure, because `-Ycheck` after erasure fails on any generic value class for a different reason, fixed separately in retronym/scala#427.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WP2). Prepared with Claude.
