#!/usr/bin/env python3
"""Seed clustering analysis of a hash-seed campaign.

usage: seeds.py <campaign>=<hashdump dir>... [--corpus scala] [--all-layouts]
(run in the lab directory, JAVA_HOME set for perflab). Several campaigns pool their forks (variant names must not clash
unless they are the same treatment). --all-layouts also counts non-seed variants (identity, hc3) as layout groups, valid
when each of them gave one layout in every fork (-Xbatch).

Per fork: instructions/op, cycles/op, ms/op, alloc/op from perflab's API, joined by pid with the probe's lite meta
(run-0 identity-hash fingerprint, uniques layout digest and probe displacement of the last op). Per variant: mean, sd,
CV over forks. Over the seedN variants: one-way ANOVA, variance components between and within seeds, ICC."""
import json, os, statistics as st, subprocess, sys, math

args = sys.argv[1:]
corpus = 'scala'
if '--corpus' in args: i = args.index('--corpus'); corpus = args[i + 1]; del args[i:i + 2]
all_layouts = '--all-layouts' in args
args = [a for a in args if a != '--all-layouts']
camps = [a.split('=', 1) for a in args]
camp = '+'.join(c for c, _ in camps)

def api(*args):
    return json.loads(subprocess.check_output(['perflab', 'api', *args]))

rows = []
for cn, hd in camps:
  for c in api('cells', '--campaign', cn, '--corpus', corpus)['cells']:
    if c.get('state') != 'done': continue
    d = api('cell', '--campaign', cn, '--corpus', corpus, '--variant', c['variant'], '--round', str(c['round']))
    m = {x['key']: x['value'] for x in d['metrics']}
    pid = next((i['pid'] for i in d['iterations'] if i.get('pid')), None)
    r = dict(variant=c['variant'], round=c['round'], pid=pid, instr=m.get('instructions'), cycles=m.get('cycles'),
             ms=m.get('score'), alloc=m.get('alloc'))
    md = os.path.join(hd, str(pid))
    if pid and os.path.isdir(md):
        metas = sorted(f for f in os.listdir(md) if f.endswith('.meta'))
        def meta(f): return dict(l.rstrip('\n').split('\t', 1) for l in open(os.path.join(md, f)))
        m0, ml = meta(metas[0]), meta(metas[-1])
        r.update(fp0=m0['startFingerprint'], udig=ml['uniquesDigest'], disp=int(ml['probeDispSum']),
                 dmax=int(ml['probeDispMax']), nuniq=int(ml['uniques']), runs=len(metas),
                 udigs=len(set(meta(f)['uniquesDigest'] for f in metas)))
    rows.append(r)

def summ(xs):
    m = st.mean(xs); s = st.stdev(xs) if len(xs) > 1 else 0.0
    return m, s, 100 * s / m

variants = []
for r in rows:
    if r['variant'] not in variants: variants.append(r['variant'])
print(f'campaign {camp}, corpus {corpus}: {len(rows)} forks')
print(f"{'variant':10} {'n':>2} {'instr/op (G)':>13} {'sd (M)':>8} {'CV%':>6} | {'ms/op':>7} {'CV%':>5} | {'alloc CV%':>9} | layouts/fp0 seen")
for v in variants:
    rs = [r for r in rows if r['variant'] == v]
    mi, si, ci = summ([r['instr'] for r in rs]); mm, sm, cm = summ([r['ms'] for r in rs]); _, _, ca = summ([r['alloc'] for r in rs])
    lay = len(set(r.get('udig') for r in rs)); fps = len(set(r.get('fp0') for r in rs))
    print(f'{v:10} {len(rs):2} {mi/1e9:13.4f} {si/1e6:8.1f} {ci:6.3f} | {mm:7.0f} {cm:5.2f} | {ca:9.4f} | {lay} layouts, {fps} run-0 fingerprints')

print('\nper fork (sorted by variant, instructions):')
for v in variants:
    for r in sorted((r for r in rows if r['variant'] == v), key=lambda r: r['instr']):
        print(f"  {v:9} r{r['round']} pid {r['pid']}: instr {r['instr']/1e9:.4f}G cycles {r['cycles']/1e9:.3f}G ms {r['ms']:.0f} alloc {r['alloc']/1e6:.2f}MB fp0 {r.get('fp0')} layout {r.get('udig')} disp {r.get('disp')} dmax {r.get('dmax')} uniq {r.get('nuniq')} layouts/run {r.get('udigs')}")

def fsf(F, d1, d2):
    """P(F(d1,d2) > F) = I_{d2/(d2+d1 F)}(d2/2, d1/2), the regularized incomplete beta by Lentz's continued fraction."""
    x, a, b = d2 / (d2 + d1 * F), d2 / 2, d1 / 2
    def cf(x, a, b):
        tiny = 1e-300; c, d = 1.0, 1 - (a + b) * x / (a + 1); d = 1 / (d if abs(d) > tiny else tiny); h = d
        for m in range(1, 300):
            for num in (m * (b - m) * x / ((a + 2*m - 1) * (a + 2*m)), -(a + m) * (a + b + m) * x / ((a + 2*m) * (a + 2*m + 1))):
                d = 1 + num * d; d = 1 / (d if abs(d) > tiny else tiny); c = 1 + num / c; c = c if abs(c) > tiny else tiny; h *= d * c
        return h
    lb = math.lgamma(a + b) - math.lgamma(a) - math.lgamma(b) + a * math.log(x) + b * math.log(1 - x)
    if x < (a + 1) / (a + b + 2): return math.exp(lb) * cf(x, a, b) / a
    return 1 - math.exp(lb) * cf(1 - x, b, a) / b

def anova(groups):
    k = len(groups); N = sum(map(len, groups)); grand = st.mean([x for g in groups for x in g])
    ssb = sum(len(g) * (st.mean(g) - grand) ** 2 for g in groups); ssw = sum(sum((x - st.mean(g)) ** 2 for x in g) for g in groups)
    msb, msw = ssb / (k - 1), ssw / (N - k)
    return k, N, grand, msb, msw, (msb / msw if msw else float('inf'))

def ranks(xs):
    o = sorted(range(len(xs)), key=lambda i: xs[i]); r = [0.0] * len(xs); i = 0
    while i < len(o):
        j = i
        while j + 1 < len(o) and xs[o[j + 1]] == xs[o[i]]: j += 1
        for t in range(i, j + 1): r[o[t]] = (i + j) / 2 + 1
        i = j + 1
    return r

def perm_p(groups, stat, n=20000):
    """Permutation p-value of stat(groups) under exchangeable fork labels."""
    import random
    rnd = random.Random(1); flat = [x for g in groups for x in g]; sizes = list(map(len, groups)); obs = stat(groups); hits = 0
    for _ in range(n):
        rnd.shuffle(flat); gs = []; i = 0
        for s in sizes: gs.append(flat[i:i + s]); i += s
        if stat(gs) >= obs - 1e-12: hits += 1
    return (hits + 1) / (n + 1)

seeds = [v for v in variants if (all_layouts or v.startswith('seed')) and sum(r['variant'] == v for r in rows) >= 2]
groups = [[r['instr'] for r in rows if r['variant'] == v] for v in seeds]
if len(groups) >= 2:
    k, N, grand, msb, msw, F = anova(groups)
    n0 = (N - sum(len(g) ** 2 for g in groups) / N) / (k - 1)
    var_seed = max(0.0, (msb - msw) / n0)
    p = fsf(F, k - 1, N - k)
    pp = perm_p(groups, lambda gs: anova(gs)[5])
    # Kruskal-Wallis H on ranks (robust to the occasional whole-fork slow or fast JIT mode), permutation p-value
    def kw(gs):
        flat = [x for g in gs for x in g]; r = ranks(flat); i = 0; h = 0.0; n = len(flat)
        for g in gs: rg = r[i:i + len(g)]; i += len(g); h += len(g) * (st.mean(rg) - (n + 1) / 2) ** 2
        return 12 * h / (n * (n + 1))
    print(f'\nlayouts: one-way ANOVA on instructions/op over {k} layouts, {N} forks: F({k-1},{N-k}) = {F:.2f}, p = {p:.3g} (permutation p = {pp:.3g})')
    print(f'  Kruskal-Wallis H = {kw(groups):.2f}, permutation p = {perm_p(groups, kw):.3g}')
    print(f'  within-seed sd {math.sqrt(msw)/1e6:.1f}M ({100*math.sqrt(msw)/grand:.3f}%), between-seed sd {math.sqrt(var_seed)/1e6:.1f}M ({100*math.sqrt(var_seed)/grand:.3f}%), ICC {var_seed/(var_seed+msw):.2f}')
    means = sorted((st.mean(g), v) for g, v in zip(groups, seeds))
    meds = sorted((st.median(g), v) for g, v in zip(groups, seeds))
    print('  layout medians, sorted (G):', ' '.join(f'{v}={m/1e9:.3f}' for m, v in meds))
    mm = [m for m, _ in meds]; gm = st.median(mm)
    print(f'  spread of layout medians: sd {100*st.stdev(mm)/gm:.3f}%, range {100*(mm[-1]-mm[0])/gm:.3f}%, IQR {100*(mm[3*len(mm)//4]-mm[len(mm)//4])/gm:.3f}%')
    print('  seed means (G):', ', '.join(f'{v} {m/1e9:.4f}' for m, v in means), f'range {(means[-1][0]-means[0][0])/grand*100:.3f}%')
    # layout covariate
    ds = [(st.mean([r['disp'] for r in rows if r['variant'] == v]), st.mean(g)) for g, v in zip(groups, seeds) if all('disp' in r for r in rows if r['variant'] == v)]
    if len(ds) >= 3:
        xs, ys = zip(*ds)
        print(f'  correlation of seed mean instructions with uniques probe displacement: r = {st.correlation(xs, ys):.2f}')

# Robust view: medians and MAD per variant (outlier forks in a slow JIT mode inflate sd), and the allocation mode.
def mad(xs):
    m = st.median(xs); return st.median([abs(x - m) for x in xs])
allocs = sorted(r['alloc'] for r in rows)
split = None
if allocs and allocs[-1] - allocs[0] > 0.01 * allocs[0]:
    gaps = [(allocs[i + 1] - allocs[i], i) for i in range(len(allocs) - 1)]
    g, i = max(gaps); split = (allocs[i] + allocs[i + 1]) / 2
print('\nrobust: median instructions/op, MAD (as % of median)' + (f'; allocation modes split at {split/1e6:.0f} MB' if split else ''))
for v in variants:
    xs = [r['instr'] for r in rows if r['variant'] == v]
    lo = sum(1 for r in rows if r['variant'] == v and split and r['alloc'] < split)
    print(f'  {v:10} median {st.median(xs)/1e9:.4f}G  MAD {100*mad(xs)/st.median(xs):.3f}%' + (f'  low-alloc forks {lo}/{len(xs)}' if split else ''))
if split:
    lo = [r['instr'] for r in rows if r['alloc'] < split]; hi = [r['instr'] for r in rows if r['alloc'] >= split]
    print(f'  by allocation mode: low {len(lo)} forks median {st.median(lo)/1e9:.4f}G, high {len(hi)} forks median {st.median(hi)/1e9:.4f}G')
