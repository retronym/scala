#!/usr/bin/env python3
# Fixed splits of the cores against adaptive scheduling, on real and synthetic graphs, with each curve.
import json, statistics as st
from sim import *

MS_PER_LINE = 0.03


def policies(budget):
    out = []
    t = 1
    while t <= 8 and budget // t >= 1:
        out.append((f'{budget // t}x{t}', ('fixed', budget // t, t)))
        t *= 2
    return out


def run(g, c, budget):
    r = {}
    for name, p in policies(budget): r[name] = simulate(g, c, budget, p)
    r['nopipe'] = simulate(g, c, budget, ('fixed', budget, 1), pipelined=False)
    r['adapt'] = simulate(g, c, budget, ('adaptive',))
    r['adaptP'] = simulate(g, c, budget, ('adaptive',), penalty=True)
    return r


def line(label, r, serial, b):
    fixed = {k: v for k, v in r.items() if 'x' in k}
    best = min(fixed, key=fixed.get)
    print(f"{label:34s} serial {serial/1000:8.1f}s | nopipe {serial/r['nopipe']:5.2f}x | " + ' '.join(f"{k} {serial/v:5.2f}x" for k, v in fixed.items()) +
          f" | adapt {serial/r['adapt']:5.2f}x adaptP {serial/r['adaptP']:5.2f}x | best fixed {best}, adaptive gains {100*(fixed[best]/r['adapt']-1):+5.1f}% ({100*(fixed[best]/r['adaptP']-1):+5.1f}% with penalty)", flush=True)


def main():
    for corpus in ('compiler', 'pekko'):
        c = Curves(corpus)
        print(f"== curves {corpus}: f={c.f:.2f} serial {c.serial:.0f}ms\n s_pre {({k: round(v,2) for k,v in c.s_pre.items()})}\n s_post {({k: round(v,2) for k,v in c.s_post.items()})}\n load {[(a, round(b,2)) for a,b in c.load_pts]}\n intrinsic pre {({k: round(v,2) for k,v in c.i_pre.items()})} post {({k: round(v,2) for k,v in c.i_post.items()})}")
        # the model against what it was fitted to
        one = Graph([c.serial], [[]])
        print(' check: one module, 8 workers:', round(simulate(one, c, 8, ('fixed', 1, 8))), 'measured', round(c.raw['full']['p8']))
        eight = Graph([c.serial] * 8, [[] for _ in range(8)])
        print(' check: 8 modules, serial:', round(simulate(eight, c, 8, ('fixed', 8, 1))), 'measured', round(c.raw['load']['k8']))
        reals = json.load(open(HERE + '/graphs.json'))
        for b in (8, 16):
            print(f'-- real graphs, {b} cores')
            for name, g in reals.items():
                G = from_real(g, MS_PER_LINE)
                line(f'{name} ({G.n})', run(G, c, b), sum(G.work), b)
            print(f'-- synthetic, {b} cores')
            for n in (200, 1000, 5000):
                for shape in ('wide', 'deep', 'hourglass', 'spine'):
                    rows = []
                    for seed in range(5):
                        G = synth(n, shape, seed, 120, 1.6)
                        rows.append((run(G, c, b), sum(G.work)))
                    keys = rows[0][0].keys()
                    # medians of speed-ups, as makespans of graphs of the same shape
                    r = {k: st.median(x[1] / x[0][k] for x in rows) for k in keys}
                    line(f'{shape} n={n}', {k: 1 / r[k] for k in keys}, 1.0, b)


if __name__ == '__main__':
    main()
