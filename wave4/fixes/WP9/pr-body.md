This pull request holds a failing test and no fix.

The program matches the literal `List[Int]()` against `case a :: tl`; ordinary code does not match on an empty list literal. With `-Ycheck:all` the compiler's tree checker reports `type mismatch; found: scala.collection.immutable.::[Int], required: scala.collection.immutable.Nil.type; then 'Tree has erroneous type: (head: Int, next: List[Int]): scala.collection.immutable.::[Int]((a @ _), (tl @ _))'`. The checker prints these as warnings and the compiler still exits 0, so the test adds `-Werror` to make the report fail the test. The program compiles and runs correctly, and there is no effect outside `-Ycheck`.

This is a checker false positive: the tree is well typed as the typer left it. The typer rewrites the scrutinee List[Int]() to gen.mkNil with type List[Int] (canTranslateEmptyListToNil in Typers), and the checker's re-typing of the Ident Nil gives Nil.type, against which the :: pattern is not possible.

Possible fix: In the checker, accept a re-typed Nil that is narrower than its original type, or have the typer's rewrite keep the type with Typed(Nil, List[T]). No compatibility cost. It belongs with retronym/scala#509, which accepts re-typings that differ for a known compiler rewrite (the renamed field). The lost class file that older bases show is the destructive checker, which retronym/scala#459 fixes.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WP9). Prepared with Claude.
