This pull request holds a failing test and no fix.

The program has a method m whose body applies a partial function literal containing `return Option(0.5)` with applyOrElse. With `-Ycheck:all` the compiler's tree checker reports `warning: [check: typer] Return symbol (method m} does not reference directly enclosing DefDef (method applyOrElse); Tree has erroneous type: scala.Option.apply[Double](0.5) (during phase superaccessors on the base used here)`. The checker prints these as warnings and the compiler still exits 0, so the test adds `-Werror` to make the report fail the test. The program compiles and runs correctly, and there is no effect outside `-Ycheck`.

This is a checker false positive: the original typing is right and the program returns 0.5 from m. Only the checker's re-typing of the Return picks the expected type of the synthetic applyOrElse (its B1) instead of the result type of the method the return targets.

Possible fix: When re-typing a Return in the checker, take the expected type from the Return symbol's result type rather than from the innermost enclosing method. The change is confined to the checker. It belongs with retronym/scala#460, which already accepts the return target for a return in a partial function literal nested in the synthetic applyOrElse but not the re-typing of its expression. The lost class file that older bases show with this program is the destructive checker, which retronym/scala#459 fixes.

Found in the fourth wave of the coordinated code review of 2.13.x (finding WP6). Prepared with Claude.
