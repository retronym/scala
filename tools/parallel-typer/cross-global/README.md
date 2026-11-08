# Cross-Global experiments

Small measurements behind `DESIGN-cross-global-sharing.md`. `run.sh` builds the argument files and the harness from `build/quick` (run `sbt dist/mkQuick` first) and takes a mode:

- `round`: one warm JVM, the same sources on a fresh `Global` and on a kept `Global`, alternating; medians per phase and a byte comparison.
- `ripple`: one edit, rounds as Zinc would run them, on a fresh `Global`, a kept one, and a kept one with the output directory invalidated, compared with a clean build.
- `profile`, `profile-kept`: async-profiler collapsed stacks of warm rounds; `shares.py` gives inclusive shares.
- `retain`: retained heap of a kept `Global` after each run.
- `phases`: per-module phase times for `critpath.py`, serially or under `-Yparallel-typer`.
- `joint`: several subprojects compiled in one `Global`, compared with the per-module outputs of `phases`.
