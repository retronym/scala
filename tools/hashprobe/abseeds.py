#!/usr/bin/env python3
"""A/B of two builds over hash seeds: one campaign per build, with the same seedN variants.

usage: abseeds.py <campaign a> <campaign b> [--corpus scala]   (run in the lab directory, JAVA_HOME set for perflab)

Per seed: instructions/op, cycles/op and alloc/op of each build (median over its forks) and b's change. Overall: the
median and mean of the per-seed changes, a sign-flip permutation p-value of the mean, and the spread of each build's
seed medians, so that a change in sensitivity to the layout shows as well as a change in level."""
import json, math, random, statistics as st, subprocess, sys

args = sys.argv[1:]
corpus = 'scala'
if '--corpus' in args: i = args.index('--corpus'); corpus = args[i + 1]; del args[i:i + 2]
a, b = args

def api(*xs): return json.loads(subprocess.check_output(['perflab', 'api', *xs]))

def load(camp):
    per = {}
    for c in api('cells', '--campaign', camp, '--corpus', corpus)['cells']:
        if c.get('state') != 'done': continue
        d = api('cell', '--campaign', camp, '--corpus', corpus, '--variant', c['variant'], '--round', str(c['round']))
        m = {x['key']: x['value'] for x in d['metrics']}
        per.setdefault(c['variant'], []).append(m)
    return {v: {k: st.median([m[k] for m in ms if m.get(k) is not None]) for k in ('instructions', 'cycles', 'alloc')} | {'n': len(ms)}
            for v, ms in per.items()}

A, B = load(a), load(b)
seeds = sorted(set(A) & set(B), key=lambda v: int(v[4:]) if v[4:].isdigit() else v)
print(f'{a} -> {b}, corpus {corpus}, {len(seeds)} seeds')
print(f"{'seed':8} {'instr a':>9} {'instr b':>9} {'d instr':>8} | {'cyc a':>7} {'cyc b':>7} {'d cyc':>7} | {'alloc a MB':>10} {'alloc b MB':>10} {'d alloc MB':>10}")
di, dc, da = [], [], []
for v in seeds:
    x, y = A[v], B[v]
    pi = 100 * (y['instructions'] / x['instructions'] - 1); pc = 100 * (y['cycles'] / x['cycles'] - 1); al = (y['alloc'] - x['alloc']) / 1e6
    di.append(pi); dc.append(pc); da.append(al)
    print(f"{v:8} {x['instructions']/1e9:9.3f} {y['instructions']/1e9:9.3f} {pi:+7.2f}% | {x['cycles']/1e9:7.2f} {y['cycles']/1e9:7.2f} {pc:+6.2f}% | {x['alloc']/1e6:10.1f} {y['alloc']/1e6:10.1f} {al:+10.1f}")

def signflip(ds, n=20000):
    r = random.Random(1); obs = abs(st.mean(ds)); hits = sum(abs(st.mean([d if r.random() < .5 else -d for d in ds])) >= obs - 1e-12 for _ in range(n))
    return (hits + 1) / (n + 1)

for name, ds in (('instructions', di), ('cycles', dc)):
    print(f'{name}: median change {st.median(ds):+.2f}%, mean {st.mean(ds):+.2f}% (sign-flip p = {signflip(ds):.3g}), '
          f'changes range {min(ds):+.2f}% .. {max(ds):+.2f}%')
print(f'alloc: median change {st.median(da):+.1f} MB/op, range {min(da):+.1f} .. {max(da):+.1f}')
for name, R in ((a, A), (b, B)):
    xs = [R[v]['instructions'] for v in seeds]; m = st.median(xs)
    print(f'{name}: seed medians of instructions/op: median {m/1e9:.3f}G, sd {100*st.stdev(xs)/m:.2f}%, range {100*(max(xs)-min(xs))/m:.2f}%, '
          f'MAD {100*st.median([abs(x-m) for x in xs])/m:.2f}%')
