import csv, json, math, sys, collections
rows = list(csv.DictReader(open(sys.argv[1])))
data = collections.defaultdict(list)
timeouts = collections.defaultdict(list)
for r in rows:
    if r['phase_ms'] == 'timeout': timeouts[(r['compiler'], r['shape'])].append(int(r['n'])); continue
    data[(r['compiler'], r['shape'])].append((int(r['n']), float(r['phase_ms']), float(r['total_ms'])))
MIN_MS = 5.0
def fit(pts):
    pts = [(n, t) for n, t in pts if t >= MIN_MS]
    if len(pts) < 2: return None
    xs = [math.log2(n) for n, _ in pts]; ys = [math.log2(t) for _, t in pts]
    mx, my = sum(xs)/len(xs), sum(ys)/len(ys)
    k = sum((x-mx)*(y-my) for x, y in zip(xs, ys)) / sum((x-mx)**2 for x in xs)
    return k
out = {}
# Scala 2 gives up the analysis ("Cannot check match", AnalysisBudget.maxFormulaSize = 2000 conjuncts) for
# matches over sealed types with more than ~2000 cases, so those points are not comparable: exclude them from fits.
def capped(c, sh, n): return (c.startswith("scala2") or (len(c) == 3 and c[0] == "c" and c[1:].isdigit())) and sh != "ints" and n >= 2048
for (c, sh), pts in sorted(data.items()):
    pts.sort()
    phase = [(n, p) for n, p, _ in pts if not capped(c, sh, n)]
    k = fit(phase)
    tail = fit(phase[-3:]) if len(phase) >= 3 else None
    out.setdefault(sh, {})[c] = {"points": [[n, p, t] for n, p, t in pts], "k": k, "k_tail": tail, "timeouts": timeouts.get((c, sh), []), "capped": [n for n, _, _ in pts if capped(c, sh, n)]}
    print(f"{sh:6s} {c:13s} k={k if k is None else round(k,2)!s:5s} tail={tail if tail is None else round(tail,2)!s:5s} " +
          " ".join(f"{n}:{p:.0f}" for n, p in phase))
json.dump(out, open(sys.argv[2], 'w'), indent=1)
