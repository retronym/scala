# Which subclasses of Global does sbt instantiate in one class loader?

Branch `instrument-global-subclasses` makes `Global`'s constructor record `getClass` in a per-class-loader set, print `[global-classes] ...` to stderr when a new class appears, and throw once the set exceeds `-Dscala.globalClasses.max` (default 2).

- `ThreeGlobals.scala`: synthetic check, three anonymous subclasses on the quick build.
- `sbt-project/`: three subprojects (`macros` with a `c.eval` macro like jsoniter-scala's `JsonCodecMaker`; `app`, `app2` use it), against the `publishLocal`ed `2.13.19-bin-SNAPSHOT`.
- `out/`: logs of the runs.

Reproduce (from the repo root, after `sbt dist/mkQuick publishLocal`):

    build/quick/bin/scalac -d /tmp/tg scratch/global-classes/ThreeGlobals.scala && build/quick/bin/scala -cp /tmp/tg ThreeGlobals
    cd scratch/global-classes/sbt-project
    sbt clean compile doc                                    # throws in doc (3rd class)
    sbt -Dscala.globalClasses.max=100 clean compile doc 'set app/Compile/console/scalacOptions += "-Xnojline"' app/console

`-Xnojline` works around a jline binary clash between sbt 1.12.15 and the 2.13.19 snapshot REPL (`NoSuchMethodError: MouseSupport.keys`), unrelated to this question.
