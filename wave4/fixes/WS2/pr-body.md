Stacked on retronym/scala#459, which this extends: it already treats existential quantifiers inside a symbol's info as bound in the `-Ycheck` scope check. Only the last commit is new here.

Under `-Ycheck`, the tree checker's scope check reports the type parameters bound inside an inferred `lub` refinement as out of scope. `def f(b: Boolean) = if (b) List(1) else Vector(1)` gets 74 "Out of scope symbol reference" warnings under `-Ycheck:typer,uncurry`, for the `_` and `A` parameters of type lambdas such as the `CC` arguments of `StrictOptimizedSeqOps` and `SeqFactory`. The type of `if (b) LazyList.empty else Nil` is the enormous refinement of scala/bug#10908; its result gets thousands of such reports, and each report rendered the whole method signature (over 800 KB of text) before truncating it to 80 characters. `-Ycheck:typer` on that one-line program and `-Ycheck:all` on `run/t498` therefore run for many minutes. Compiled code is not affected.

`checkSymbolRefsRespectScope` collects the symbols referenced in a definition's info and reports those whose owner does not enclose the tree. Type parameters of a polymorphic member or type lambda inside the type are owned by that member or by the original class's type parameter, so the owner walk can never succeed for them, although they are bound in the type itself.

The check now treats the type parameters of every `PolyType` in the checked type as bound, as retronym/scala#459 does for existential quantifiers, and reports all out-of-scope references of one tree in one message, so the tree, its type and its enclosing definitions are rendered once per tree rather than once per reference.

This is not a regression: the check and its message date from 2013, and the large lub from the 2.13 collections. Rendering a signature in full before truncating it is still costly for a genuine report on such a type; printing with a length bound would avoid that, but the printer has no such mode.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WS2). Prepared with Claude.
