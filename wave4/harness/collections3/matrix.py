#!/usr/bin/env python3
# usage: matrix.py <alias.tsv> : one markdown table per family and method; rows are classes, columns are sources.
import sys, collections
short = {"ok": "ok", "CME": "CME", "WRONG": "WRONG", "n/a": "-"}
tables = collections.OrderedDict()
for line in open(sys.argv[1]):
    fam, impl, method, source, verdict = line.rstrip("\n").split("\t")[:5]
    t = tables.setdefault((fam, method), (collections.OrderedDict(), []))
    if source not in t[1]: t[1].append(source)
    t[0].setdefault(impl, {})[source] = short.get(verdict, verdict.replace("EXC:", "").replace("Exception", "Ex"))
print("Legend: ok = result equals the snapshot model; CME = ConcurrentModificationException; WRONG = returns a different state without an exception; HANG = no result after 2 s, or the heap was exhausted; an exception name = any other exception; - = the class has no such method.\n")
for (fam, method), (rows, cols) in tables.items():
    rows = {k: v for k, v in rows.items() if any(x != "-" for x in v.values())}
    if not rows: continue
    print(f"### {fam}: {method}\n")
    print("| class | " + " | ".join(cols) + " |")
    print("|---|" + "---|" * len(cols))
    for impl, r in rows.items():
        print(f"| {impl} | " + " | ".join(r.get(c, "") for c in cols) + " |")
    print()
