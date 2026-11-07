#!/usr/bin/env python3
# A fluid simulation of a clean multi-module build on a budget of cores, from measured scaling curves (curves.sh).
# run.py compares fixed splits of the cores with adaptive scheduling on real and synthetic graphs; choke.py sweeps the
# share of the work in a chain of chokepoint modules. Warm JITs throughout, and adaptive workers move instantly.
#
# A module has W ms of serial work: a share f before its pickles, the rest after. A phase on T workers, with B workers
# busy in the whole build, runs at intrinsic(T) / load(B) serial-seconds per second. load(B) is how N independent
# compiles slow each other (measured, young generation sized for the load); intrinsic(T) is the parallel compiler's
# speed-up at T threads with the machine's own share at T taken out, so that it is not charged twice. T=0 is the serial
# compiler: rate 1 / load(B), one worker.
import json, math, os, random, statistics as st

HERE = os.path.dirname(os.path.abspath(__file__))
TS = [1, 2, 4, 6, 8]


def out_rows(name):
    rows = {}
    for l in open(f'{HERE}/curves/{name}.out'):
        if l.startswith('R '):
            r = json.loads(l[2:]); rows.setdefault(r['token'], []).append(r['batch'])
    return {k: st.median(v) for k, v in rows.items()}


class Curves:
    def __init__(self, corpus):
        full, pre, load = out_rows(f'{corpus}-full'), out_rows(f'{corpus}-pre'), out_rows(f'{corpus}-load')
        self.f = pre['k1'] / full['k1']
        self.serial = full['k1']
        post = {t: max(1.0, full[f'p{t}'] - pre[f'p{t}']) for t in TS}
        postS = full['k1'] - pre['k1']
        self.s_pre = {t: pre['k1'] / pre[f'p{t}'] for t in TS}
        self.s_post = {t: postS / post[t] for t in TS}
        self.load_pts = sorted((int(k[1:]), v / load['k1']) for k, v in load.items())
        self.raw = dict(full=full, pre=pre, load=load)
        self.i_pre = {t: self.s_pre[t] * self.load(t) for t in TS}
        self.i_post = {t: self.s_post[t] * self.load(t) for t in TS}

    def load(self, b):
        p = self.load_pts
        if b <= p[0][0]: return p[0][1]
        for (a, x), (c, y) in zip(p, p[1:]):
            if b <= c: return x + (y - x) * (b - a) / (c - a)
        return p[-1][1] * b / p[-1][0]

    def intrinsic(self, phase, t):
        """Speed at t workers, t=0 the serial compiler."""
        if t == 0: return 1.0
        c = self.i_pre if phase == 0 else self.i_post
        if t in c: return c[t]
        lo = max(x for x in TS if x <= t) if t >= 1 else 1
        hi = min((x for x in TS if x >= t), default=None)
        if hi is None: return c[8]
        return c[lo] + (c[hi] - c[lo]) * (t - lo) / (hi - lo)


class Graph:
    def __init__(self, work, deps, hard=None, f=None):
        self.n = len(work); self.work = work; self.deps = deps
        self.f = f # each module's measured share of its work before its pickles; else the curve's
        self.hard = hard or [set() for _ in deps] # edges that need the dependency's class files, for macros
        self.rdeps = [[] for _ in work]
        for m, ds in enumerate(deps):
            for d in ds: self.rdeps[d].append(m)

    def order(self):
        ind = [len(d) for d in self.deps]; q = [i for i, x in enumerate(ind) if not x]; out = []
        while q:
            m = q.pop(); out.append(m)
            for r in self.rdeps[m]:
                ind[r] -= 1
                if not ind[r]: q.append(r)
        return out


def priorities(g, f, pipelined):
    fs = g.f or [f] * g.n
    return _priorities(g, fs, pipelined)


def _priorities(g, fs, pipelined):
    """Longest serial time from this module's start to the end of the build along what waits on it."""
    rem = [0.0] * g.n
    for m in reversed(g.order()):
        w = g.work[m]; best = w
        for r in g.rdeps[m]:
            wait = w if (not pipelined or m in g.hard[r]) else fs[m] * w
            best = max(best, wait + rem[r])
        rem[m] = best
    return rem


def simulate(g, c, budget, policy, pipelined=True, penalty=False, tmax=8):
    """policy: ('fixed', M, T) or ('adaptive',). Returns the makespan in ms."""
    fs = g.f or [c.f] * g.n
    pri = priorities(g, c.f, pipelined)
    n = g.n
    pre_left = [w * f for w, f in zip(g.work, fs)]; post_left = [w * (1 - f) for w, f in zip(g.work, fs)]
    wait = [0] * n # dependencies still to reach what this module needs
    for m in range(n):
        wait[m] = len(g.deps[m])
    state = [0] * n # 0 waiting, 1 ready, 2 pre, 3 post, 4 done
    ready = set(m for m in range(n) if wait[m] == 0)
    for m in ready: state[m] = 1
    running = {} # module -> workers (for fixed), reassigned for adaptive
    now = 0.0; done = 0
    fixed = policy[0] == 'fixed'
    static = policy[0] == 'static'
    balanced = policy[0] == 'balanced'
    M = policy[1] if fixed else None; T = policy[2] if fixed else None

    def release(m, full):
        for r in g.rdeps[m]:
            need_full = not pipelined or m in g.hard[r]
            if need_full == full:
                wait[r] -= 1
                if wait[r] == 0: state[r] = 1; ready.add(r)

    def assign():
        order = sorted(ready, key=lambda m: -pri[m])
        if static:
            # as XrayBuild's adapt<C>: a module takes its workers when it starts and keeps them until it ends
            for m in order:
                free = budget - sum(running.values())
                if free <= 0: break
                top = max(pri[x] for x in range(n) if state[x] != 4)
                t = min(tmax, free) if pri[m] >= 0.9 * top or len(ready) == 1 else 1
                ready.discard(m); state[m] = 2; running[m] = t
            return
        if balanced:
            # as XrayBuild's bal<C>
            for m in order:
                free = budget - sum(running.values())
                if free <= 0: break
                left = sum(g.work[x] for x in range(n) if state[x] != 4)
                target = left / (budget * 0.65)
                t = next((t for t, sp in ((1, 1.0), (2, 1.75), (3, 2.4), (4, 3.0), (6, 3.6), (8, 4.0)) if g.work[m] / sp <= target), 8)
                if len(ready) == 1: t = tmax
                t = min(t, free)
                ready.discard(m); state[m] = 2; running[m] = t
            return
        if fixed:
            free = M - len(running)
            for m in order[:max(0, free)]:
                ready.discard(m); state[m] = 2; running[m] = T
            return
        # every running compile keeps a worker; new ones start while a worker is free; the rest go where each adds the most
        left = budget - len(running)
        cap = policy[1] if len(policy) > 1 else budget # at most so many compiles at once
        for m in order:
            if left <= 0 or len(running) >= cap: break
            ready.discard(m); state[m] = 2; running[m] = 1; left -= 1
        for m in running: running[m] = 1
        while left > 0:
            bestm, bestv = None, 0.0
            for m, t in running.items():
                if t >= tmax: continue
                ph = 0 if state[m] == 2 else 1
                base = 0 if t == 1 and not penalty else t
                weight = pri[m] if ph == 0 else 1.0 # the back end only when nothing else can use the worker
                speed = (lambda w: c.intrinsic(ph, w, m)) if hasattr(c, 'by_module') else (lambda w: c.intrinsic(ph, w))
                # in proportion to the work waiting: a module with twice the work gets the same gain from twice the speed
                gain = weight * (math.log(speed(t + 1)) - math.log(speed(base)))
                if gain > bestv: bestm, bestv = m, gain
            if bestm is None: break
            running[bestm] += 1; left -= 1

    while done < n:
        assign()
        if not running: raise RuntimeError('deadlock')
        busy = sum(running.values())
        load = c.load_at(busy, len(running)) if hasattr(c, 'load_at') else c.load(busy)
        dt = math.inf; rates = {}
        for m, t in running.items():
            ph = 0 if state[m] == 2 else 1
            # one worker is the serial compiler, unless the compile is in parallel mode and pays for it (`penalty`)
            w = 0 if t == 1 and not penalty else t
            rate = c.intrinsic(ph, w, m) / load if hasattr(c, 'by_module') else c.intrinsic(ph, w) / load
            rates[m] = rate
            left = pre_left[m] if ph == 0 else post_left[m]
            dt = min(dt, left / rate)
        now += dt
        for m in list(running):
            r = rates[m] * dt
            if state[m] == 2:
                pre_left[m] -= r
                if pre_left[m] <= 1e-6:
                    state[m] = 3; release(m, False)
            else:
                post_left[m] -= r
                if post_left[m] <= 1e-6:
                    state[m] = 4; done += 1; del running[m]; release(m, True)
    return now


def lower_bounds(g, c, budget):
    best_pre, best_post = max(c.i_pre[t] / c.load(t) for t in TS), max(c.i_post[t] / c.load(t) for t in TS)
    # the chain of pickles at the best speed any compile can reach
    rem = [0.0] * g.n
    for m in reversed(g.order()):
        w = g.work[m]; best = w * (c.f / best_pre + (1 - c.f) / best_post)
        for r in g.rdeps[m]:
            wt = w / best_pre if m in g.hard[r] else c.f * w / best_pre
            best = max(best, wt + rem[r])
        rem[m] = best
    return dict(path=max(rem), throughput=sum(g.work) * c.load(budget) / budget)


def synth(n, shape, seed, median_ms, sigma):
    """A layered graph. shape: 'wide' many shallow layers of leaves over a small core; 'hourglass' a choke of big platform
    modules part way up; 'spine' a chain of big modules everything hangs off, as a platform monorepo; 'deep' narrow layers."""
    r = random.Random(seed)
    work = [median_ms * math.exp(r.gauss(0, sigma)) for _ in range(n)]
    layers = []
    if shape == 'wide':
        depth = max(4, int(math.log2(n)))
        widths = [max(1, int(n * (1.6 ** i) / sum(1.6 ** j for j in range(depth)))) for i in range(depth)]
    elif shape == 'deep':
        depth = max(8, int(n ** 0.5)); widths = [n // depth] * depth
    elif shape == 'hourglass':
        depth = max(6, int(1.5 * math.log2(n))); mid = depth // 2
        widths = [1 if abs(i - mid) <= 1 else max(2, n // depth) for i in range(depth)]
    elif shape == 'spine':
        s = max(6, int(n ** 0.5 / 2)); depth = s + 3
        widths = [1] * s + [max(1, (n - s) // 3)] * 3
    i = 0
    for w in widths:
        layers.append(list(range(i, min(n, i + w)))); i += w
    if i < n: layers[-1] += list(range(i, n))
    layers = [l for l in layers if l]
    deps = [[] for _ in range(n)]
    for li, l in enumerate(layers):
        if li == 0: continue
        for m in l:
            k = 1 + min(r.randint(0, 5), 6)
            for _ in range(k):
                # mostly the layer below, sometimes further down
                back = min(li, 1 + int(r.expovariate(1.0)))
                deps[m].append(r.choice(layers[li - back]))
            deps[m] = sorted(set(deps[m]))
    if shape in ('hourglass', 'spine'):
        # the choke modules are the platform: bigger than their neighbours
        for li, l in enumerate(layers):
            if len(l) == 1: work[l[0]] = max(work[l[0]], 8 * median_ms)
    return Graph(work, deps)


def from_real(g, ms_per_line, hard_macros=True):
    names = list(g); ix = {m: i for i, m in enumerate(names)}
    return Graph([g[m]['lines'] * ms_per_line for m in names], [[ix[d] for d in g[m]['deps']] for m in names],
                 [set(ix[d] for d in g[m]['macroDeps']) if hard_macros else set() for m in names])

