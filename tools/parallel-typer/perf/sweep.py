#!/usr/bin/env python3
"""Medians of repeated x-ray sweeps: wall and thread CPU per step, per variant and configuration.
   sweep.py <dir>/<variant>/<corpus>/r<N>/run-<config>.json ...; prints a table per corpus, by config."""
import json, os, re, statistics as st, sys, collections
runs = collections.defaultdict(list)  # (corpus, variant, config) -> [(wall ms, cpu ms, {step: cpu ms})]
for f in sys.argv[1:]:
    parts = f.split(os.sep)
    variant, corpus, config = parts[-4], parts[-3], re.sub(r'run-(.*)\.json', r'\1', parts[-1])
    d = json.load(open(f))
    if int(d['meta'].get('errors', '0')) > 0: print('ERRORS', f, file=sys.stderr)
    strs, sp = d['strings'], d['spans']
    steps = collections.Counter()
    # outermost spans only: on the pool a unit has a span of the step and, inside it, one of applyPhase
    outer = {}
    for i in sorted(range(0, len(sp), 10), key=lambda i: (sp[i + 3], -sp[i + 4])):
        if sp[i + 4] <= outer.get(sp[i], -1): continue
        outer[sp[i]] = sp[i + 4]
        steps[strs[sp[i + 1]]] += sp[i + 5] / 1000
    runs[corpus, variant, config].append((float(d['meta']['compileMs']), sum(steps.values()), steps))
order = {'serial': 0}
key = lambda c: order.get(c, int(c) if c.isdigit() else 99)
for corpus in sorted({k[0] for k in runs}):
    variants = sorted({k[1] for k in runs if k[0] == corpus})
    print(f'\n## {corpus}: median wall ms (min-max) / thread CPU in spans ms, n={len(next(v for k,v in runs.items() if k[0]==corpus))}')
    for v in variants:
        sw = st.median(r[0] for r in runs[corpus, v, 'serial'])
        for c in sorted({k[2] for k in runs if k[0] == corpus and k[1] == v}, key=key):
            rs = runs[corpus, v, c]; w = [r[0] for r in rs]
            print(f'  {v:16} {c:7} wall {st.median(w):7.0f} ({min(w):.0f}-{max(w):.0f})  speed-up {sw / st.median(w):5.2f}x  cpu {st.median(r[1] for r in rs):7.0f}')
    print('\n  thread CPU by step (median ms):')
    allsteps = collections.Counter()
    for k, rs in runs.items():
        if k[0] == corpus:
            for r in rs: allsteps.update(r[2])
    top = [s for s, _ in allsteps.most_common(14)]
    print('  ' + ' ' * 25 + ''.join(f'{s[:10]:>11}' for s in top))
    for v in variants:
        for c in sorted({k[2] for k in runs if k[0] == corpus and k[1] == v}, key=key):
            rs = runs[corpus, v, c]
            print(f'  {v:16} {c:7} ' + ''.join(f'{st.median(r[2][s] for r in rs):11.0f}' for s in top))
