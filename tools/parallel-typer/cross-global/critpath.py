#!/usr/bin/env python3
"""Critical path of scala/scala's module graph under handoff models, from the PHASES lines of RoundBench's `phases` mode.

critpath.py <phases file> [signature share of the typer, default 0.2] [cores, default 6] [lowering speed-up, default 1] [modules to skip, comma separated]

Models, for downstream D of upstream U:
  traditional  D starts when U has written class files.
  pipelined    D starts when U's pickler ends, the early output today.
  signature    D parses at once, indexes when U has indexed, runs its signature stage when U's is done,
               and runs its post-typer steps when U's are done (expanded names, see the design note).
  lockstep     one universe: build-wide steps, each divided by the cores except the index and half of the
               signature stage (class headers and annotations), which are serial in the spike.
The signature share is an estimate unless taken from a -Dscalac.parallelTyper.log run.
"""
import sys

DEPS = {'library': [], 'reflect': ['library'], 'compiler': ['reflect'], 'interactive': ['compiler'],
        'repl': ['compiler', 'interactive'], 'replFrontend': ['repl'], 'scaladoc': ['compiler'], 'scalap': ['compiler']}
FRONT = ['parser', 'namer', 'packageobjects', 'typer', 'superaccessors', 'extmethods', 'pickler']
SIG = float(sys.argv[2]) if len(sys.argv) > 2 else 0.2
CORES = int(sys.argv[3]) if len(sys.argv) > 3 else 6
LOWER = float(sys.argv[4]) if len(sys.argv) > 4 else 1.0  # assumed speed-up of lowering, a what-if for lowering on a pool
SKIP = set(sys.argv[5].split(',')) if len(sys.argv) > 5 else set()


def load(f):
    d = {}
    for l in open(f):
        if not l.startswith('PHASES'):
            continue
        _, _, m, tot, ps = l.rstrip('\n').split('\t')
        p = {k: float(v) for k, v in (x.split('=') for x in ps.split())}
        d[m] = dict(parse=p['parser'], index=p.get('namer', 0) + p.get('packageobjects', 0), typer=p['typer'],
                    post=p.get('superaccessors', 0) + p.get('extmethods', 0) + p.get('pickler', 0),
                    back=sum(v for k, v in p.items() if k not in FRONT) / LOWER, total=float(tot))
        d[m]['total'] = d[m]['parse'] + d[m]['index'] + d[m]['typer'] + d[m]['post'] + d[m]['back']
    return {m: x for m, x in d.items() if m not in SKIP}


def sim(d, model):
    done = {}
    for m in DEPS:
        x = d.get(m)
        if x is None:
            continue
        sig, body = SIG * x['typer'], (1 - SIG) * x['typer']
        deps = [u for u in DEPS[m] if u in done]
        mx = lambda k: max([done[u][k] for u in deps], default=0)
        if model == 'signature':
            ix = max(x['parse'], mx('index')) + x['index']
            s = max(ix, mx('sig')) + sig
            pk = max(s + body, mx('pickle')) + x['post']
        else:
            t = mx('end') if model == 'traditional' else mx('pickle')
            ix = t + x['parse'] + x['index']
            s = ix + sig
            pk = s + body + x['post']
        done[m] = dict(index=ix, sig=s, pickle=pk, end=pk + x['back'])
    return max(v['end'] for v in done.values())


def lockstep(d):
    w = lambda k: sum(x[k] for x in d.values())
    sig = SIG * w('typer')
    return (w('parse') + w('post') + w('back') * LOWER + (w('typer') - sig) + sig / 2) / CORES + w('index') + sig / 2


d = load(sys.argv[1])
work = sum(x['total'] for x in d.values())
w = lambda k: sum(x[k] for x in d.values())
print(f'{sys.argv[1]}: work {work:.0f} ms over {len(d)} modules; work/{CORES} cores {work / CORES:.0f} ms; signature share {SIG}')
for m, x in d.items():
    print(f"  {m:13s} parse {x['parse']:6.0f} index {x['index']:5.0f} typer {x['typer']:6.0f} post {x['post']:5.0f} back {x['back']:6.0f} total {x['total']:6.0f}")
for mo in ('traditional', 'pipelined', 'signature'):
    t = sim(d, mo)
    print(f'  {mo:12s} critical path {t:7.0f} ms; with the core bound {max(t, work / CORES):7.0f} ms')
print(f'  lockstep on {CORES} cores {lockstep(d):7.0f} ms')
F = (w('parse') + w('index') + w('typer') + w('post')) / work
B = w('back') / work
S = SIG * w('typer') / work
print(f'  fractions of work: front end {F:.2f} (signature stage {S:.2f}), lowering {B:.2f}')
for L in (2, 4, 8, 16):
    print(f'  chain of {L:2d} equal modules, in units of one module: traditional {L:5.2f}, pipelined {L * F + B:5.2f}, signature {L * S + F - S + B:5.2f}, core bound {L / CORES:5.2f}')
