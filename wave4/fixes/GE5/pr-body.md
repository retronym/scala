This pull request holds a failing test and no fix.

A lambda that reads a local lazy val of a specialized type, in a body that specialization duplicates, is rejected:

```scala
class C {
  def m[@specialized(Int) B](p: B): () => B = { lazy val x: B = p; () => x }
}
```

fails with `type mismatch; found: Int, required: x.type`. The same body in a method of `class D[@specialized(Int) B]` fails too. A plain val, or a local def or local class that reads the lazy val, compiles. The program is well typed and `new C().m(1)()` should be `1`; Scala 3 compiles it.

Specialization copies the body into the `Int` variant and retypes it with `Duplicators`. The message says the lambda's body keeps the singleton type `x.type` of the reference as its expected type, while the retyped reference to the lazy val in the `Int` variant has type `Int`. Not a regression: 2.12 rejects it the same way. No ticket found; scala/bug#8473 and #8122 are open tickets in the same area with other triggers.

Possible fix: in `Duplicators`, retype the reference to the local lazy val inside the `Function` so that its type is not checked against the stale singleton type.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GE5). Prepared with Claude.
