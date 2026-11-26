#!/usr/bin/env python3
"""Select the seed programs: single-file run/jvm tests with a .check file; write name<TAB>path<TAB>options, and excluded.tsv."""
import os, re, sys
root = "/Users/jzaugg-virtus/code/.worktrees/scala/w3-base/test/files"
out = sys.argv[1]
keep, excl = [], []
for d in ("run", "jvm"):
    for f in sorted(os.listdir(f"{root}/{d}")):
        if not f.endswith(".scala"): continue
        n = f[:-6]; p = f"{root}/{d}/{f}"
        if not os.path.exists(f"{root}/{d}/{n}.check"): continue
        src = open(p, errors="replace").read()
        head = src.splitlines()[:10]
        opts = []; why = None
        for l in head:
            m = re.match(r"\s*//>\s*using\s+(\S+)\s*(.*)", l)
            if m:
                if m.group(1) == "options": opts += m.group(2).split()
                else: why = "directive " + m.group(1)
        if why is None:
            if re.search(r"System\.exit|sys\.exit|Runtime\.getRuntime|\bexit\(", src): why = "exits"
            elif re.search(r"StdIn|System\.in\b|readLine|Source\.stdin|\.stdin", src): why = "stdin"
            elif "scala.tools.partest" in src or "tools.partest" in src: why = "partest class"
            elif os.path.exists(f"{root}/{d}/{n}.flags") or os.path.exists(f"{root}/{d}/{n}.javaopts") or os.path.exists(f"{root}/{d}/{n}.args"): why = "flags/javaopts/args file"
            elif os.path.isdir(f"{root}/{d}/{n}") or any(x.startswith(n + "_") for x in os.listdir(f"{root}/{d}")): why = "multi-file"
        (excl if why else keep).append((d + "/" + n, p, " ".join(opts), why))
with open(out, "w") as o:
    for n, p, op, _ in keep: o.write(f"{n}\t{p}\t{op}\n")
with open(out + ".excluded", "w") as o:
    for n, p, op, w in excl: o.write(f"{n}\t{w}\n")
print(len(keep), "kept", len(excl), "excluded")
