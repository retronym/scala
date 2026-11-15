import sys, os
D = sys.argv[1]
forks = sorted(p for p in os.listdir(D) if p.isdigit())
def runs(f): return sorted(int(x[4:7]) for x in os.listdir(os.path.join(D,f)) if x.endswith('.meta'))
def load(f, n, kind):
    with open(os.path.join(D,f,'run-%03d.%s'%(n,kind))) as fh: return [l.rstrip('\n').split('\t') for l in fh]
def meta(f,n): return dict(l for l in load(f,n,'meta'))
common = sorted(set.intersection(*[set(runs(f)) for f in forks]))
print('##', os.path.basename(D), 'forks', forks, 'runs', [len(runs(f)) for f in forks])
cache={}
def T(f,n):
    k=(f,n)
    if k not in cache: cache[k]=(load(f,n,'trees.tsv'), load(f,n,'uniques.tsv'))
    return cache[k]
tot_runs=0; full=0; fracs=[]
for n in common:
    t0,u0=T(forks[0],n); m0=meta(forks[0],n)
    cells=[]; allsame=True
    for f in forks[1:]:
        t,u=T(f,n); m=meta(f,n)
        ft=sum(1 for x,y in zip(t0,t) if x[5]==y[5])/len(t0) if len(t)==len(t0) else -1
        km=sum(1 for x,y in zip(t0,t) if x[:5]!=y[:5])
        fu=sum(1 for x,y in zip(u0,u) if x[0]==y[0] and x[2]==y[2])/len(u0) if len(u)==len(u0) else -1
        fracs.append(ft)
        if ft<1 or fu<1 or km: allsame=False
        cells.append('%.4f/%.4f%s'%(ft,fu,'' if not km else ' km=%d'%km))
    tot_runs+=1; full+=allsame
    ms=[meta(f,n) for f in forks]
    fp=' '.join('%s>%s tid%s gc%s-%s @%s'%(m['startFingerprint'],m['endFingerprint'],m.get('threadId'),m.get('startGc'),m.get('endGc'),m.get('startUptimeMs')) for m in ms)
    print('%3d %s %s | %s'%(n,'==' if allsame else '!=',' '.join(cells),fp))
print('runs fully identical across all forks: %d/%d; mean tree-hash agreement %.4f'%(full,tot_runs,sum(fracs)/len(fracs)))
# within fork drift
for f in forks:
    rs=runs(f); prev=None; out=[]
    for n in rs:
        t,_=T(f,n) if n in common else (load(f,n,'trees.tsv'),None)
        if prev is not None: out.append('%.3f'%(sum(1 for x,y in zip(prev,t) if x[5]==y[5])/len(t)))
        prev=t
    print('drift', f, 'agreement with previous run: min %s max %s'%(min(out),max(out)))
