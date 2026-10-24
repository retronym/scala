# Reproducing the profile figures

Profiles: one JMH fork per build, recorded by perflab with JMH's async-profiler integration, which profiles the measured iterations only (warmup and trial setup are outside the recording):

```
perflab run --profile async:cpu --out out/compiler-lab/runs/<name> -- \
  -jvm <jdk26>/bin/java -p corpus=re2s -p globalClasses=4 -p variants=<name>=@build=../<build> \
  -wi 18 -i 12 -w 3 -r 3 -f 1 CompileBenchmark
```

perflab passes `-prof async:output=jfr;event=cpu;interval=1000000;rawCommand=cstack=vm,features=mixed+vtable+comptask`: CPU samples every 1 ms, mixed Java/native stacks (so HotSpot's `itable stub`, `vtable stub` and `lookup_secondary_supers_table_slow_path_stub` appear as frames), and `vtable` adds a pseudo-frame naming the receiver class under each dispatch stub.

Benchmark thread only, as collapsed stacks:

```
jfrconv --cpu --threads -I '.*jmh-worker.*' -o collapsed <run>/<cell>/jfr-cpu.jfr <name>.collapsed
python3 stubs2.py <name>.collapsed   # itable/vtable stub shares, receivers and callers
python3 stubs.py  <name>.collapsed   # slow path share and callers, top self frames
```

A share is samples whose stack contains the stub frame (itable/vtable) or ends in the slow-path stub, over all benchmark-thread samples. The summaries of every run are in `../profiles/`.

Pre-filtered flame graphs (stacks through a stub, cut to its three nearest callers, reversed):

```
jfrconv --cpu --simple -I lookup_secondary_supers_table_slow_path_stub -o collapsed <jfr> full.collapsed
# keep, for each stack, the stub, the frame after it and the three frames before it, then:
jfrconv -r --title "... (4.8% of all samples) ..." -o html top.collapsed top.html
```
