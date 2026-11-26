A field initialiser with a local val or a pattern match, in a class with a specialized field, crashes the compiler:

```scala
class C[@specialized(Int) A](val a: A) {
  val b: Int = Option(a) match { case Some(x) if x == a => 1; case Some(_) => 2; case None => 3 }
}
class D[@specialized(Int) A](val a: A) {
  val b: Boolean = { val t = Option(a); t.isEmpty }
}
```

`C` fails with `EmptyScope.enter` during constructors; `D` fails in the back end with "Error while emitting" and a `NullaryMethodType` reaching it. Both should compile; `new C(1).b` is `1` and `new D(1).b` is `false`. These are scala/bug#7574 (since 2.10) and scala/bug#10096; scala/bug#8172 and scala/bug#8320 were closed as their duplicates. The examples from both tickets are in the test. Not a regression.

When the constructor of a class with specialized fields uses a specialized member, the constructor statements of the generic class are copied into the specialized subclass's constructor: `Constructors.mergeConstructors` retypes each with `Duplicators`. It did so in the template's typer context, whose owner is the class, so a block-local val, or a match temporary such as `x1`, was re-entered as a member of the subclass (printed as `private[this] val x1`, referenced as `C$mcI$sp.this.x1`) instead of a local of the constructor. In `D` the back end then met a member it could not emit. In `C` an assignment that the match translation makes to its temporary was retyped as a setter call, the typer fell back to an implicit search after typer, and that search entered into the empty scope.

The statements are now retyped in a context owned by the subclass's primary constructor, with a fresh scope, so their locals stay locals of the constructor they are copied into. scala/bug#8156 (a pattern definition `var arne @ (pelle, ceasar) = ...` in a specialized class, "value x$2 is not a member of Arne$mcI$sp[U]") is a different defect and still fails.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GE7). Prepared with Claude.
