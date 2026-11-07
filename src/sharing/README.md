# scala-sharing, vendored

The concurrency primitives of [scala-sharing](https://github.com/retronym/scala-sharing), relocated from `io.github.retronym.sharing` to `scala.reflect.internal.sharing`, for the parallel typer (`-Yparallel-typer`, see PLAN-parallel-typer.md), copied from the Scala 2.13 build at 33614c2439378df3f8709657b6e3aed6c2e0defb:

- `src/sharing`: the library's `src/` (the runtime shared by both builds) and `src-2/` (the 2.13 API, whose operations are def macros). Compiled into `scala-reflect` (the `reflect` project adds this directory to its sources), so it ships in scala-reflect.jar.
- `src/sharing-macros`: the library's `macros-2/src/`, the macro implementations: the `sharingMacros` project, compiled by the reference compiler (STARR) and a compile-time dependency of `reflect` and `compiler`, whose sources call the macros. It is not shipped.

The relocation matters: a top-level package `io` in scala-reflect.jar shadows `scala.io` for code in the empty package or in a package nested in the root, so `io.Codec` stops compiling (scalap failed this way under the re-STARRed compiler).

Copy again with: `cp -R <scala-sharing>/src/io/github/retronym/sharing/* <scala-sharing>/src-2/io/github/retronym/sharing/* src/sharing/scala/reflect/internal/sharing/; cp -R <scala-sharing>/macros-2/src/io/github/retronym/sharing/macros/* src/sharing-macros/scala/reflect/internal/sharing/macros/`, then relocate: `grep -rl io.github.retronym src/sharing src/sharing-macros | xargs sed -i -e 's/io\.github\.retronym\.sharing/scala.reflect.internal.sharing/g' -e 's/^package io\.github\.retronym$/package scala.reflect.internal/'`, and update the SHA above. Do not otherwise edit the copies: change the library and copy again.
