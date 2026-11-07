#!/usr/bin/env python3
# The simulator against real builds (XrayBuild.scala): feed it each module's serial time and pickle time from `trad:1x1`
# and compare what it predicts for each token with what the build measured.
#
#   calibrate.py <XrayBuild output> <module file> [curve corpus...]
import json, os, re, statistics as st, sys
from sim import *


def rows(path):
    return [json.loads(l[2:]) for l in open(path) if l.startswith('R ')]


def modfile(path):
    mods, cur = {}, None
    for l in open(path):
        k, v = l[:1], l[2:].strip()
        if k == 'M': cur = mods[v] = dict(deps=[], up=[], macros=False, lines=0)
        elif k == 'D': cur['deps'].append(v)
        elif k == 'U': cur['up'].append(v)
        elif k == 'P': cur['macros'] = True
        elif k == 'L': cur['lines'] = int(v)
    return mods


def serial_times(rs, token='trad:1x1'):
    w, p = {}, {}
    for r in rs:
        if r['token'] == token:
            for m in r['mods']:
                w.setdefault(m['n'], []).append(m['e'] - m['s']); p.setdefault(m['n'], []).append(m['p'] - m['s'])
    return {n: (st.median(v), st.median(p[n])) for n, v in w.items()}


def graph(mods, times):
    names = list(mods); ix = {n: i for i, n in enumerate(names)}
    work = [max(times[n][0], 0.01) for n in names]
    f = [min(1.0, times[n][1] / max(times[n][0], 0.01)) for n in names]
    deps, hard = [], []
    for n in names:
        ds = {ix[d] for d in mods[n]['deps'] if d in ix}
        # as XrayBuild: class files of whatever on the classpath provides macros
        hs = {ix[u] for u in mods[n]['up'] if u in ix and mods[u]['macros']}
        deps.append(sorted(ds | hs)); hard.append(hs)
    return Graph(work, deps, hard, f), names


def policy(token):
    strategy, p = token.split(':')
    if p.startswith('adapt'): return strategy == 'pipe', int(p[5:]), ('static', int(p[5:]))
    if p.startswith('bal'): return strategy == 'pipe', int(p[3:]), ('balanced', int(p[3:]))
    m, t = map(int, p.split('x'))
    return strategy == 'pipe', m * t, ('fixed', m, t)


def main():
    rs = rows(sys.argv[1]); mods = modfile(sys.argv[2])
    times = serial_times(rs); g, names = graph(mods, times)
    serial = st.median(r['batch'] for r in rs if r['token'] == 'trad:1x1')
    print(f'modules {g.n}, serial sum {sum(g.work)/1000:.1f} s, median whole build {serial/1000:.1f} s, pickle share {sum(w*f for w,f in zip(g.work,g.f))/sum(g.work):.2f}')
    tokens = []
    for r in rs:
        if r['token'] not in tokens: tokens.append(r['token'])
    print(f"{'token':14s} {'measured':>10s} {'x serial':>8s} " + ' '.join(f'{c:>16s}' for c in sys.argv[3:] or ['compiler', 'pekko']))
    for t in tokens:
        b = st.median(r['batch'] for r in rs if r['token'] == t)
        pipelined, budget, pol = policy(t)
        cells = []
        for corpus in sys.argv[3:] or ['compiler', 'pekko']:
            c = Curves(corpus)
            p = simulate(g, c, budget, pol, pipelined=pipelined)
            cells.append(f'{p/1000:7.1f} {100*(p/b-1):+6.0f}%')
        print(f"{t:14s} {b/1000:9.1f}s {serial/b:7.2f}x " + ' '.join(f'{x:>16s}' for x in cells))


if __name__ == '__main__':
    main()
