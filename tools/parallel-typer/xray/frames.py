#!/usr/bin/env python3
# Compare CPU profiles by frame (`FRAMES=<rounds> profile.sh`), per compilation's worth of samples.
#
#   frames.py <dir> <repeats> [--step <regex>] [--self | --total] [--top N] [--match <regex>] [--vs <dir> <config>] [a b]
#
# A line of the collapsed stacks is [thread];frames...;[step/unit...] n. The table has a frame's self (or, with --total,
# inclusive, counted once per stack) samples per compilation in each configuration, sorted by the difference, the
# column of each step's samples first with --steps.
import argparse, collections, glob, os, re

ap = argparse.ArgumentParser()
ap.add_argument("dir"); ap.add_argument("repeats", type=int)
ap.add_argument("--step"); ap.add_argument("--total", action="store_true"); ap.add_argument("--top", type=int, default=40)
ap.add_argument("--match"); ap.add_argument("--steps", action="store_true"); ap.add_argument("--threads", action="store_true")
ap.add_argument("--workers", action="store_true", help="only the compiler's threads, not GC or JIT")
ap.add_argument("--under", help="only stacks through a frame matching this")
ap.add_argument("configs", nargs="*", default=["serial", "1"])
a = ap.parse_args()

def lines(d, c):
    fs = sorted(glob.glob(os.path.join(d, "cmp-%s-*.collapsed" % c)))
    for f in fs:
        for l in open(f, errors="replace"):
            stack, _, n = l.rstrip("\n").rpartition(" ")
            yield stack.split(";"), int(n)

def parse(stack):
    thread = stack[0] if stack[0].startswith("[") else ""
    frames = stack[1:] if thread else stack
    tags = None
    # --spans: the tags of the enclosing spans, as a last frame marked _[s]
    if frames and frames[-1].endswith("_[s]"):
        tags = frames[-1][:-4].strip()
        frames = frames[:-1]
    return thread, [re.sub(r"_\[.\]$", "", f) for f in frames], tags

def isCompilerThread(t):
    return bool(re.search(r"main|scalac-", t))

def collect(d, c):
    n = len(glob.glob(os.path.join(d, "cmp-%s-*.collapsed" % c))) * a.repeats or 1
    res = collections.Counter()
    for stack, cnt in lines(d, c):
        thread, frames, tags = parse(stack)
        if not frames: continue
        if a.workers and not isCompilerThread(thread): continue
        step = (tags or "(no span)").split(" > ")[0].split(";")[0].split("/")[0].strip()
        if a.step and not re.search(a.step, step): continue
        if a.under and not any(re.search(a.under, f) for f in frames): continue
        if a.threads: res[re.sub(r"tid=\d+|-\d+|#\d+", "", thread)] += cnt
        elif a.steps: res[step] += cnt
        elif a.total:
            for f in set(frames): res[f] += cnt
        else: res[frames[-1]] += cnt
    return {k: v / n for k, v in res.items()}, sum(1 for _ in [0]) and n

tables = [collect(a.dir, c)[0] for c in a.configs]
keys = set().union(*[t.keys() for t in tables])
if a.match: keys = {k for k in keys if re.search(a.match, k)}
rows = sorted(keys, key=lambda k: -abs(tables[-1].get(k, 0) - tables[0].get(k, 0)))[: a.top]
print("%10s%10s%9s  frame" % (a.configs[0], a.configs[-1], "diff"))
for k in rows:
    x, y = tables[0].get(k, 0), tables[-1].get(k, 0)
    print("%10.1f%10.1f%+9.1f  %s" % (x, y, y - x, k[:130]))
if not a.total:
    print("%10.1f%10.1f%+9.1f  all" % (sum(tables[0].values()), sum(tables[-1].values()), sum(tables[-1].values()) - sum(tables[0].values())))
