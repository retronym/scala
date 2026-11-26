A lambda that reads a local lazy val of a specialized type, in a body that specialization duplicates, is rejected:

```scala
class C {
  def m[@specialized(Int) B](p: B): () => B = { lazy val x: B = p; () => x }
}
```

fails with `type mismatch; found: Int, required: x.type`. The same body in a method of `class D[@specialized(Int) B]` fails too, as does a local def returning such a lambda. The program is well typed and `new C().m(1)()` should be `1`; Scala 3 compiles it.

Typer gives the reference `x` the singleton type `x.type`, and the lambda the type `() => x.type`. Uncurry lifts the lambda body into `def $anonfun$m(): x.type = x`. The fields phase then turns the local lazy val into a lazy accessor method, so the body becomes the call `x()`, still typed `x.type`. When specialization duplicates the method for `Int`, `Duplicators` keeps the declared result type `x.type` (with `x` mapped to the copy's accessor) but retypes the call `x()` from scratch, which gives `Int`: a call is not a stable path, so typer cannot give it the singleton type again, and `Int` does not conform to `x.type`.

`Duplicators.fixType` now widens a singleton type of a local lazy accessor to its underlying type before substituting the specialization environment, so `$anonfun$m` declares `Int` in the `Int` variant. Erasure gives such a singleton type the erasure of its underlying type anyway.

Not a regression: 2.12 rejects it the same way. No ticket found; scala/bug#8473 and scala/bug#8122 are open tickets on `Duplicators` with other triggers.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GE5). Prepared with Claude.
