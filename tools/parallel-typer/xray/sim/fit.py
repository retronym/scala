#!/usr/bin/env python3
# A cost model fitted to real builds. Each module has its own speed-up by thread count, from builds that compile one
# module at a time (1xT). Compiles slow each other by what the machine loses to busy threads and what the collector loses
# to live sets: load = 1 + a (busy - 1)^p + c (compiles - 1)^q. Fitted on 8x1, 16x1, 4x2 and 2x8, which separate the two;
# every other token is held out. With no module graph's macro edges and with workers that move, it also estimates what
# no real run here can show.
#
#   fit.py <module file> <XrayBuild output>...
import json, statistics as st, sys
from sim import Graph, simulate
from calibrate import graph, modfile, policy, rows, serial_times

TRAIN = ('pipe:8x1', 'pipe:16x1', 'pipe:4x2', 'pipe:2x8')
WIDTHS = (2, 4, 6, 8)


class Fitted:
    by_module = True

    def __init__(self, names, table, a, p, cc, q=1.0):
        self.names, self.table, self.a, self.p, self.cc, self.q = names, table, a, p, cc, q
        self.f = None

    def load_at(self, busy, compiles):
        return 1 + self.a * max(busy - 1, 0) ** self.p + self.cc * max(compiles - 1, 0) ** self.q

    def load(self, busy):
        return self.load_at(busy, 1)

    def intrinsic(self, phase, t, m=None):
        if t == 0: return 1.0
        if t <= 1: return 0.93 # parallel mode at one thread
        c = self.table[m][phase]
        if t >= WIDTHS[-1]: return c[WIDTHS[-1]]
        lo = max(w for w in WIDTHS if w <= t); hi = min(w for w in WIDTHS if w >= t)
        return c[lo] if lo == hi else c[lo] + (c[hi] - c[lo]) * (t - lo) / (hi - lo)

    # one compile alone at T has the machine's share at T in its speed-up already
    def rebase(self):
        for m, phases in self.table.items():
            for ph in phases:
                for t in phases[ph]:
                    phases[ph][t] *= self.load_at(t, 1)


def tables(rs, names, times):
    """Each module's speed-up at each width against its serial time: before the pickles, and after them."""
    out = {}
    durations = {}
    for r in rs:
        for m in r['mods']:
            durations.setdefault(r['token'], {}).setdefault(m['n'], []).append((m['p'] - m['s'], m['e'] - m['p']))
    # the build's overall speed-up at a width stands in for a module too small to measure
    overall = {}
    for t in WIDTHS:
        tok = f'pipe:1x{t}'
        if tok in durations:
            overall[t] = sum(times[n][0] for n in names) / sum(st.median(a + b for a, b in durations[tok][n]) for n in names)
    for i, n in enumerate(names):
        w, pre = times[n]
        post = max(w - pre, 0.01)
        phases = {0: {}, 1: {}}
        for t in WIDTHS:
            tok = f'pipe:1x{t}'
            if w < 100:
                phases[0][t] = phases[1][t] = 1.0
                continue
            if tok not in durations:
                continue # filled in below, between its neighbours
            a = st.median(x[0] for x in durations[tok][n]); b = st.median(x[1] for x in durations[tok][n])
            phases[0][t] = pre / max(a, 1.0); phases[1][t] = post / max(b, 1.0)
        for ph in phases:
            if 6 not in phases[ph]: phases[ph][6] = (phases[ph][4] + phases[ph][8]) / 2
        out[i] = phases
    return out


def main():
    mods = modfile(sys.argv[1])
    latest = {}
    for path in sys.argv[2:]:
        by = {}
        for r in rows(path): by.setdefault(r['token'], []).append(r)
        latest.update(by)
    rs = [r for v in latest.values() for r in v]
    times = serial_times(rs); g, names = graph(mods, times)
    measured = {t: st.median(r['batch'] for r in v) for t, v in latest.items()}
    base = tables(rs, names, times)
    best = None
    copy = lambda: json.loads(json.dumps(base), object_hook=lambda o: {int(k) if k.lstrip('-').isdigit() else k: v for k, v in o.items()})
    for a in (0, .002, .005, .01, .02, .03, .05):
        for p in (1.0, 1.5):
            for cc in (0, .002, .005, .01, .02, .03, .05, .08, .12):
                for q in (1.0, 1.5, 2.0):
                    c = Fitted(names, copy(), a, p, cc, q)
                    c.rebase()
                    err = 0
                    for t in TRAIN:
                        pipelined, budget, pol = policy(t)
                        err += (simulate(g, c, budget, pol, pipelined=pipelined) / measured[t] - 1) ** 2
                    if best is None or err < best[0]: best = (err, a, p, cc, q)
    _, a, p, cc, q = best
    print(f'fit: a={a} p={p} c={cc} q={q}, on {TRAIN}')
    c = Fitted(names, copy(), a, p, cc, q)
    c.rebase()
    res = {}
    for t, b in measured.items():
        pipelined, budget, pol = policy(t)
        pred = simulate(g, c, budget, pol, pipelined=pipelined)
        res[t] = round(pred)
        role = 'train' if t in TRAIN else 'single-width' if t.startswith('pipe:1x') or t == 'trad:1x1' else 'held out'
        print(f'{t:14s} measured {b/1000:7.1f} predicted {pred/1000:7.1f} {100*(pred/b-1):+6.0f}%  {role}')
    held = [abs(res[t] / measured[t] - 1) for t in measured if t not in TRAIN and not t.startswith('pipe:1x') and t not in ('trad:1x1', 'trad:1x8')]
    print(f'held out: mean absolute error {100*st.mean(held):.1f}%, worst {100*max(held):.1f}%')
    # what no real run here can show: workers that move, and the same graph with no macro providers
    est = {}
    for b in (8, 16):
        for cap in (2, 3, 4, 6, 8, 12, 16):
            if cap > b: continue
            est[f'dynamic{b}cap{cap}'] = round(simulate(g, c, b, ('adaptive', cap)))
        top = min((k for k in est if k.startswith(f'dynamic{b}cap')), key=est.get)
        print(f'estimate, workers that move, {b} cores:', {k.split('cap')[1]: round(est[k] / 1000, 1) for k in est if k.startswith(f'dynamic{b}cap')}, f'best fixed measured {min(v for t, v in measured.items() if policy(t)[1] == b)/1000:.1f} s')
    open_graph = Graph(g.work, g.deps, [set() for _ in g.deps], g.f)
    for t in ('pipe:8x1', 'pipe:4x2', 'pipe:4x4'):
        pipelined, budget, pol = policy(t)
        withm, without = simulate(g, c, budget, pol, pipelined=True), simulate(open_graph, c, budget, pol, pipelined=True)
        trad = simulate(g, c, budget, pol, pipelined=False)
        est[t + ':nomacros'] = round(without); est[t + ':trad'] = round(trad); est[t + ':model'] = round(withm)
        print(f'{t}: traditional {trad/1000:.1f}, pipelined {withm/1000:.1f}, pipelined with no macro waits {without/1000:.1f}')
    json.dump(dict(predicted=res, a=a, p=p, c=cc, q=q, train=list(TRAIN), heldMeanPct=round(100 * st.mean(held), 1), heldWorstPct=round(100 * max(held), 1), estimates=est), open('fit.json', 'w'))


if __name__ == '__main__':
    main()
