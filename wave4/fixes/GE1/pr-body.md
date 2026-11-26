A super call to a method with a `@specialized` method type parameter crashes the compiler when it goes through a super accessor:

```scala
trait T0[A] { def m[@specialized(Int) B](p: B): Int = 1 }
trait T1[A] extends T0[A] { override def m[@specialized(Int) B](p: B): Int = super.m[B](p) + 1 }
class C extends T1[String]
```

fails in specialize with `java.util.NoSuchElementException: key not found: method super$m`. The override is not needed (a method `n` in `T1` calling `super.m(p)` crashes too), the parent may be a class (a decorator trait `Quoted extends Show` overriding `show[@specialized(Int) A]` with `super.show(a)`), and a super call from a lambda in a class, which also goes through a super accessor, crashes the same way. The programs should compile; `new C().m(1)` is 2.

A super call in a trait, or from a lambda, is compiled as a call to a super accessor `super$m` that superaccessors creates. The accessor's info is cloned from `m`'s, so its type parameter keeps the `@specialized` annotation, and `normalizeMember` gives the accessor specialized variants such as `super$m$mIc$sp`. Their bodies are to be duplicated from the accessor's, which it does not have. `specializeMember` already skips super accessors; `normalizeMember`, which handles method type parameters, did not.

`normalizeMember` now skips super accessors too. The specialized variant of the method making the super call calls the generic accessor, boxing the argument. `specializeClass` makes the same choice for super accessors in specialized subclasses, which it does not specialize either.

A regression from 2.12: 2.12.20 compiles the trait program and prints 2, every 2.13 release tried crashes; the change responsible was not identified (`normalizeMember` and the accessor's creation are unchanged between them). No ticket found; scala/bug#4012 is the closed ancestor for super calls in specialized traits. Related open pull requests on super calls into specialized parents, with other causes: retronym/scala#354, retronym/scala#456, retronym/scala#649.

Found in the fourth wave of the coordinated code review of 2.13.x (finding GE1). Prepared with Claude.
