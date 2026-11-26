This pull request holds a failing test and no fix.

A partial function literal in a body that specialization duplicates is rejected after typer:

```scala
class C {
  def m[@specialized(Int) B](p: B): Option[B] =
    ({ case x if x > 1 => p }: PartialFunction[Int, B]).lift(2)
}
```

fails with `type mismatch; found: x1.type (with underlying type Int), required: A1`. The guard is not needed: `ys.collect { case 1 => p }` in a method with a `@specialized` type parameter fails the same way, and so does `ys.collectFirst { case s: String => p }` in a method of a specialized class whose signature mentions the class's specialized type parameter. The program is well typed and should compile and print `Some(1)`; Scala 3 compiles it.

This is scala/bug#8633, open since 2.11.0 (scala/bug#8694 was closed as its duplicate). Specialization copies the method body into each variant and retypes it with `Duplicators`. By then the literal has been expanded into an anonymous class whose `applyOrElse[A1 <: Int, B1 >: B](x1: A1, default: A1 => B1)` matches on `x1`; the error comes from retyping that method's body, where a tree of type `x1.type` is checked against `A1`. Not a regression: 2.12 behaves the same.

Possible fix: the ticket links a fix made in the miniboxing plugin, which its author said ports directly to `Duplicators`: keep the synthesized `applyOrElse` and its type parameters consistent when the anonymous class is duplicated, or retype its body with the original types. Related: retronym/scala#431 is another `Duplicators` retyping rejection.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GE4). Prepared with Claude.
