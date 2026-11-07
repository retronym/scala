#!/usr/bin/env python3
# N independent compilations at once against one alone (XrayScale.scala), with each round's collections read from the
# JVM's log. Appends one JSON line per round to --results and prints a line per configuration.
#
#   scale.py shared --args A --heap 12g --tokens k1,k2,k4,k6,k8 [--rounds 5] [--warmup 12]    one JVM
#   scale.py fork   --args A --heap 4g --n 8 [--rounds 5] [--warmup 12]                       N JVMs, started together
#
# Environment: JAVA, SNAP (the classpath: compiler snapshot, its jars and XrayScale's classes), JAVA_OPTS, RESULTS, TAG.
# A round is void if the machine slept in it: the wall clock moved and the monotonic clock did not.
import argparse, json, os, re, shlex, statistics, subprocess, sys, tempfile, time

GC = re.compile(r'^\[(\d+)ms\]\[gc\s*\] GC\(\d+\) Pause (Young|Full) \((.*?)\) (\d+)M->(\d+)M\((\d+)M\) ([\d.]+)ms')
SP = re.compile(r'^\[(\d+)ms\]\[safepoint\s*\] Safepoint "([^"]+)".*Total: (\d+) ns')


def read_log(path):
    gcs, sps = [], []
    with open(path) as f:
        for line in f:
            m = GC.match(line)
            if m:
                gcs.append(dict(t=int(m[1]), kind=m[2], cause=m[3], before=int(m[4]), after=int(m[5]), cap=int(m[6]), ms=float(m[7])))
                continue
            m = SP.match(line)
            if m:
                sps.append(dict(t=int(m[1]), op=m[2], ms=int(m[3]) / 1e6))
    return gcs, sps


def window(log, t0, t1):
    gcs, sps = log
    # the System.gc that precedes a round can log within the millisecond of its start
    g = [x for x in gcs if t0 <= x['t'] <= t1 and x['cause'] != 'System.gc()']
    s = [x for x in sps if t0 <= x['t'] <= t1 and x['op'] != 'ParallelGCCollect']
    return dict(
        young=sum(x['kind'] == 'Young' for x in g), full=sum(x['kind'] == 'Full' for x in g),
        fullCauses=[x['cause'] for x in g if x['kind'] == 'Full'],
        pauseMs=sum(x['ms'] for x in g), largestMs=max((x['ms'] for x in g), default=0.0),
        stoppedMs=sum(x['ms'] for x in s), safepoints=len(s),
        afterMB=[x['after'] for x in g], beforeMB=[x['before'] for x in g], capMB=max((x['cap'] for x in g), default=0))


def jvm(a, log):
    return [os.environ.get('JAVA', 'java'), '-Xss64m', f'-Xms{a.heap}', f'-Xmx{a.heap}', '-XX:+UseParallelGC',
            f'-Xlog:gc,safepoint:file={log}:tm,tags', '-Dxray.full=true', *shlex.split(os.environ.get('JAVA_OPTS', '')),
            '-cp', os.environ['SNAP'], 'XrayScale']


def parse(line):
    return json.loads(line[2:]) if line.startswith('R ') else None


def emit(a, rec):
    rec.update(tag=os.environ.get('TAG', ''), corpus=os.path.basename(a.args), heap=a.heap)
    rec['void'] = abs((rec['t1'] - rec['t0']) - rec['nanoMs']) > 500
    with open(os.environ['RESULTS'], 'a') as f:
        f.write(json.dumps(rec) + '\n')


def shared(a):
    with tempfile.TemporaryDirectory(dir=os.environ.get('LOGDIR')) as d:
        log = f'{d}/gc.log'
        cmd = jvm(a, log) + [a.args, str(a.warmup), str(a.rounds)] + a.tokens.split(',')
        p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        rs = []
        for line in p.stdout:
            r = parse(line)
            if r: rs.append(r)
            else: print(line.rstrip(), flush=True)
        status = p.wait()
        l = read_log(log)
        for r in rs:
            r.update(window(l, r['t0'], r['t1']), mode='shared', n=len(r['walls']))
            emit(a, r)
        # what stays after a System.gc between rounds: the classpath in memory and whatever else survives a compilation
        base = [x['after'] for x in l[0] if x['kind'] == 'Full' and x['cause'] == 'System.gc()']
        print(f'exit {status}; retained after System.gc: {base[-1] if base else "?"} MB', flush=True)
        if status: sys.exit(status)


def fork(a):
    with tempfile.TemporaryDirectory(dir=os.environ.get('LOGDIR')) as d:
        ps = []
        for i in range(a.n):
            cmd = jvm(a, f'{d}/gc{i}.log') + ['--child', a.args, str(a.warmup)]
            ps.append(subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, bufsize=1))

        def until(p, want):
            for line in p.stdout:
                if line.startswith(want): return line
                if not line.startswith('warm-up'): print(line.rstrip(), flush=True)
                elif p is ps[0]: print(line.rstrip(), flush=True)
            sys.exit(f'a child ended waiting for {want}')

        for p in ps: until(p, 'ready')
        rounds = []
        for r in range(1, a.rounds + 1):
            for p in ps: p.stdin.write('gc\n'); p.stdin.flush()
            for p in ps: until(p, 'collected')
            t0 = time.monotonic()
            for p in ps: p.stdin.write('go\n'); p.stdin.flush()
            ones = [parse(until(p, 'R ')) for p in ps]
            rounds.append((r, (time.monotonic() - t0) * 1000, ones))
        for p in ps: p.stdin.close(); p.wait()
        logs = [read_log(f'{d}/gc{i}.log') for i in range(a.n)]
        for r, wall, ones in rounds:
            ws = [window(l, o['t0'], o['t1']) for l, o in zip(logs, ones)]
            rec = dict(token=f'k{a.n}', round=r, batch=wall, mode='fork', n=a.n,
                       walls=[o['walls'][0] for o in ones], cpus=[o['cpus'][0] for o in ones], allocs=[o['allocs'][0] for o in ones],
                       totalAlloc=sum(o['totalAlloc'] for o in ones), errors=sum(o['errors'] for o in ones),
                       procCpu=sum(o['procCpu'] for o in ones), gcCpu=sum(o['gcCpu'] for o in ones),
                       t0=min(o['t0'] for o in ones), t1=max(o['t1'] for o in ones), nanoMs=max(o['nanoMs'] for o in ones),
                       load=ones[0]['load'], young=sum(w['young'] for w in ws), full=sum(w['full'] for w in ws),
                       fullCauses=[c for w in ws for c in w['fullCauses']],
                       pauseMs=sum(w['pauseMs'] for w in ws), largestMs=max(w['largestMs'] for w in ws),
                       stoppedMs=sum(w['stoppedMs'] for w in ws), safepoints=sum(w['safepoints'] for w in ws),
                       afterMB=[x for w in ws for x in w['afterMB']], beforeMB=[x for w in ws for x in w['beforeMB']],
                       capMB=max(w['capMB'] for w in ws))
            emit(a, rec)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('mode', choices=['shared', 'fork'])
    ap.add_argument('--args', required=True); ap.add_argument('--heap', required=True)
    ap.add_argument('--tokens', default='k1,k2,k4,k6,k8'); ap.add_argument('--n', type=int, default=1)
    ap.add_argument('--rounds', type=int, default=5); ap.add_argument('--warmup', type=int, default=12)
    a = ap.parse_args()
    (shared if a.mode == 'shared' else fork)(a)


if __name__ == '__main__':
    main()
