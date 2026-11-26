#!/usr/bin/env python3
"""compare.py <out dir> <seed list> [other out dir for 2.13.18 base]: print differences of each configuration against base."""
import os, re, sys, glob, collections
out = sys.argv[1]; lst = sys.argv[2]
other = sys.argv[3] if len(sys.argv) > 3 else None
def norm(s):
    s = re.sub(r"@[0-9a-f]{5,8}\b", "@H", s)
    s = re.sub(r"\$\$Lambda\S*", "$$Lambda", s)
    s = re.sub(r"\$anonfun\$[\w$]*\$\d+", "$anonfun", s)
    s = re.sub(r"\$anon\$\d+", "$anon", s)
    s = re.sub(r"\.(scala|java):\d+", r".\1:N", s)
    s = re.sub(r"\b\d+(\.\d+)? ?(ms|ns|us|s)\b", "T", s)
    return s
def read(d, n, c):
    p = f"{d}/{n}.{c}.txt"
    if not os.path.exists(p): return None
    t = open(p, errors="replace").read()
    k, _, body = t.partition("\n")
    return k, body
names = [l.split("\t")[0] for l in open(lst)]
cfgs = sorted({os.path.basename(p).split(".")[-2] for p in glob.glob(out + "/*.txt")} - {"base"})
tab = collections.Counter(); res = []
for n in names:
    fn = n.replace("/", "_")
    b = read(out, fn, "base")
    if b is None: continue
    tab["base:" + b[0].split(":")[0]] += 1
    if b[0] not in ("ok",) and not b[0].startswith("throw:"): continue
    for c in cfgs + (["2.13.18"] if other else []):
        o = read(out, fn, c) if c != "2.13.18" else read(other, fn, "base")
        if o is None: continue
        if o[0] == "skip": tab[c + ":skip"] += 1; continue
        if o == b: tab[c + ":same"] += 1; continue
        if o[0] == b[0] and norm(o[1]) == norm(b[1]): tab[c + ":same-after-norm"] += 1; continue
        tab[c + ":DIFF"] += 1; res.append((n, c, b[0], o[0]))
for k in sorted(tab): print(k, tab[k])
print()
for n, c, bk, ok in res: print(f"{n}\t{c}\tbase={bk}\tvariant={ok}")
