In a joint Scala/Java compilation on 2.13.19, a match on a sealed Java interface or class written without a `permits` clause gets no exhaustivity warning, and a match with two or more type cases gets a false "unreachable code" warning, which fails a `-Werror` build. It happens whenever no class or interface in that Java file carries `final`, `abstract`, `private` or `protected`, which covers the usual Java 17 algebraic data type:

```java
public sealed interface S {
  record A() implements S {}
  record B() implements S {}
  enum E implements S { X }
  non-sealed class C implements S {}
}
```

`s match { case _: S.A => 0; case _: S.B => 1; case _: S.E => 2 }` reports `unreachable code` at the second case instead of `match may not be exhaustive ... C()`. Compiled against the class files, or on 2.13.18, it reports the missing case.

`JavaParsers.classDecl` and `interfaceDecl` test `mods.hasFlag(SEALED)` where `SEALED` resolves to the token constant `JavaTokens.SEALED` (45, from `CommonTokens`), whose bits are `PROTECTED | PRIVATE | ABSTRACT | FINAL`, not `Flags.SEALED`. So the per-unit set `patmat.javaClassesByUnit` was created for a file only when some type in it had one of those modifiers, never because of `sealed`. The mistake dates from scala/scala#10348 (2.13.11) and was harmless while that set only chose which classes to force; scala/scala#11329 made it the source of a sealed Java type's children when there is no `permits` clause, so a missing set now means no children. The pattern matcher's analysis is correct for the children it is given.

The fix writes `Flags.SEALED` at both sites. The test is a sealed interface whose implementors are a record, an enum and a `non-sealed` class, none of them final, since the existing tests pass through `final class` cases. Only warnings change.

Regression in the unreleased 2.13.19 from scala/scala#11329. No ticket found.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GJ1). Prepared with Claude.
