This pull request holds a failing test and no fix.

The program is `by(lz)` where `by(x: => A)` and `lz` is a lazy val of type A declared in a generic trait, read from a subclass. With `-Ycheck:all` the compiler's tree checker reports `type mismatch; found: C.this.lz.type (with underlying type A), required: C.this.lz.type; then 'Tree has erroneous type: C.this.lz()' at tailcalls`. The checker prints these as warnings and the compiler still exits 0, so the test adds `-Werror` to make the report fail the test. The program compiles and runs correctly, and there is no effect outside `-Ycheck`.

The by-name thunk's lifted method is declared with the singleton type C.this.lz.type, which after the fields phase is the type of a non-stable accessor method, while its right-hand side has type A. The checker is right: the tree is ill-typed, though erasure drops the singleton type so the generated code is correct.

Possible fix: Widen the by-name thunk's body type (in uncurry, or in the typer when the expected type is abstract) so the lifted method's result type is A. This is the same widening of a by-name thunk's type that the third part of retronym/scala#460 does for a constant result in the temporary of a by-name named argument, and the fix belongs with that pull request.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WP4). Prepared with Claude.
