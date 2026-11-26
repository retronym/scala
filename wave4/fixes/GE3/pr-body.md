A specialized type parameter that a method mentions only under a by-name parameter breaks specialization in two ways:

```scala
abstract class AC[@specialized(Long) A] { def m(p: Int, q: => A): Int = p }
class C extends AC[Long] { override def m(p: Int, q: => Long): Int = p + 1 }
```

is rejected with "double definition: `override def m(p: Int, q: => Long)` and `override def m(p: Int, q: () => Long)` have same type after erasure", and

```scala
trait M[@specialized(Int) K, @specialized(Int) V] { def getOrElseUpdate(k: K, default: => V): Unit }
```

crashes with `assertion failed: sym: M$mcII$sp.getOrElseUpdate$mcII$sp target: M.getOrElseUpdate$mcI$sp`. Both are ordinary programs (a specialized map or cache with a by-name default) and should compile.

`specializedTypeVars` answers which specialized type variables a method mentions. For an original symbol it reads the info entering typer, where a by-name parameter has type `=> T`; `<byname>`'s type parameter is not specialized, so T does not count. Elsewhere it reads the info at specialize, after uncurry, where the parameter has type `() => T`, and `Function0`'s result type parameter is specialized, so T counts. Names come from the first view and some decisions from the second:

- `specialOverrides` decides from the overridden method's info after uncurry that `C.m` needs a special override, and names it from the typer view, in which `AC.m` has no specialized type variables. The special override gets the unsuffixed name `m`, and clashes with `C.m` after erasure.
- In `M`, `getOrElseUpdate` is specialized on K only and gets the variant `getOrElseUpdate$mcI$sp`. In the specialized subclass `M$mcII$sp`, the clone of that variant was created after uncurry, so its "entering typer" info is the uncurried one and it is named for K and V, `getOrElseUpdate$mcII$sp`; its forwarding target does not exist.

A by-name parameter now does not count in either view. That is what the typer view, and so every method compiled until now, already had, so the change creates no new specialized variants; by reading the uses of `specializedTypeVars`, it should change no class file of a program that compiles today. A method whose specialized method type parameter occurs only under a by-name parameter, `def m[@specialized(Int) X](q: => X)`, now gets the warning "type X is unused or used in non-specializable positions", as `def m[@specialized(Int) X](q: Seq[X])` does already; it never had specialized variants.

The first program is rejected the same way by 2.12; the second compiles on 2.12.20 and crashes on every 2.13 release tried, a regression whose responsible change was not identified. No ticket found; scala/bug#10018 and scala/bug#4436 fail the same assertion from a different cause (a bounded specialized method type parameter).

Found in the fourth wave of the coordinated code review of 2.13.x (finding GE3). Prepared with Claude.
