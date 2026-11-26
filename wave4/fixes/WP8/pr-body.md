This pull request holds a failing test and no fix.

The program is a generic method `m[B](p: B, n: Int): Option[B]` with a non-local `return Some(p)` from a closure passed to foreach. With `-Ycheck:all` the compiler's tree checker reports `type mismatch; found: Option[B(in method m)], required: Option[(some other)B(in method m)]; then 'Tree has erroneous type: ex.value()' at fields`. The checker prints these as warnings and the compiler still exits 0, so the test adds `-Werror` to make the report fail the test. The program compiles and runs correctly, and there is no effect outside `-Ycheck`.

The tree is really ill-typed, by a harmless inconsistency made by uncurry. After uncurry the parameter p has the skolem of B from the original typing, but the NonLocalReturnControl handler it builds is typed with the method's type parameter B itself, so the handler's value does not conform to the skolemized body.

Possible fix: Build the NonLocalReturnControl handler in UnCurry.nonLocalReturnTry from the (skolemized) result type of the right-hand side rather than from the method's finalResultType. Only types visible to the checker change. It belongs with retronym/scala#509, whose third part deals with skolems from the original typing in the checker.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WP8). Prepared with Claude.
