#!/usr/bin/env python3
# Compare recordings of `ROUNDS=<n> run.sh` step by step: per step and configuration, the median over the rounds of the
# wall and thread CPU summed over the step's spans, and for the whole compile its wall time.
#
#   rounds.py <dir of run-<config>-<i>.json> [config, default every one found]... [--vs <other dir> <config>]
#
# Times are in ms. A step is what the x-ray calls one: a phase's per-unit work, or a step of the parallel driver.
import glob, json, os, re, statistics, sys

def load(d, config):
    runs = []
    for f in sorted(glob.glob(os.path.join(d, "run-%s-*.json" % config))):
        r = json.load(open(f))
        strings = r["strings"]
        wall, cpu = {}, {}
        s = r["spans"]
        # a span inside another of its thread (a phase's unit on the pool is two) counts once
        ends = {}
        for i in sorted(range(0, len(s), 10), key=lambda i: (s[i + 3], -s[i + 4])):
            if s[i + 4] <= ends.get(s[i], -1): continue
            ends[s[i]] = s[i + 4]
            step = strings[s[i + 1]]
            wall[step] = wall.get(step, 0) + (s[i + 4] - s[i + 3]) / 1000
            cpu[step] = cpu.get(step, 0) + s[i + 5] / 1000
        gc = r["gc"]
        gcms = sum(gc[i + 2] - gc[i + 1] for i in range(0, len(gc), 3)) / 1000
        phases = {}
        p = r["phases"]
        for i in range(0, len(p), 3):
            phases[strings[p[i]]] = (p[i + 2] - p[i + 1]) / 1000
        runs.append(dict(wall=wall, cpu=cpu, phases=phases, total=float(r["meta"]["compileMs"]), gc=gcms, alloc=float(r["meta"].get("allocatedMB", 0))))
    return runs

def med(runs, f):
    return statistics.median(f(r) for r in runs)

def table(columns):
    # columns: list of (title, runs)
    steps = []
    for _, runs in columns:
        for r in runs:
            for k in r["wall"]:
                if k not in steps: steps.append(k)
    print("%-22s" % "" + "".join("%18s" % t for t, _ in columns))
    print("%-22s" % "step" + "".join("%9s%9s" % ("wall", "cpu") for _ in columns))
    rows = []
    for k in steps:
        rows.append((k, [(med(rs, lambda r: r["wall"].get(k, 0)), med(rs, lambda r: r["cpu"].get(k, 0))) for _, rs in columns]))
    rows.sort(key=lambda x: -max(c for _, c in x[1]))
    for k, cells in rows:
        print("%-22s" % k + "".join("%9.1f%9.1f" % c for c in cells))
    print("%-22s" % "sum of steps" + "".join("%9.1f%9.1f" % (med(rs, lambda r: sum(r["wall"].values())), med(rs, lambda r: sum(r["cpu"].values()))) for _, rs in columns))
    print("%-22s" % "GC pauses" + "".join("%9.1f%9s" % (med(rs, lambda r: r["gc"]), "") for _, rs in columns))
    print("%-22s" % "allocated MB" + "".join("%9.0f%9s" % (med(rs, lambda r: r["alloc"]), "") for _, rs in columns))
    print("%-22s" % "compile (median)" + "".join("%9.1f%9s" % (med(rs, lambda r: r["total"]), "") for _, rs in columns))
    print("%-22s" % "compile (min)" + "".join("%9.1f%9s" % (min(r["total"] for r in rs), "") for _, rs in columns))
    print("%-22s" % "rounds" + "".join("%9d%9s" % (len(rs), "") for _, rs in columns))
    print("\nphases, wall (median):")
    names = []
    for _, rs in columns:
        for r in rs:
            for k in r["phases"]:
                if k not in names: names.append(k)
    for k in names:
        print("%-22s" % k + "".join("%9.1f%9s" % (med(rs, lambda r: r["phases"].get(k, 0)), "") for _, rs in columns))

args = sys.argv[1:]
d = args.pop(0)
extra = []
if "--vs" in args:
    i = args.index("--vs")
    extra = [(args[i + 2] + " " + os.path.basename(args[i + 1].rstrip("/")), load(args[i + 1], args[i + 2]))]
    args = args[:i]
configs = args or sorted({re.match(r"run-(.*)-\d+\.json", os.path.basename(f)).group(1) for f in glob.glob(os.path.join(d, "run-*-*.json"))}, key=lambda c: (c != "serial", c))
table([(c, load(d, c)) for c in configs] + extra)
