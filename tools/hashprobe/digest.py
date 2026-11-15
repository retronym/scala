import os,sys
D=sys.argv[1]
forks=sorted(p for p in os.listdir(D) if p.isdigit())
def meta(f,n): return dict(l.rstrip('\n').split('\t',1) for l in open('%s/%s/run-%03d.meta'%(D,f,n)))
runs={f:sorted(int(x[4:7]) for x in os.listdir(os.path.join(D,f)) if x.endswith('.meta')) for f in forks}
common=sorted(set.intersection(*map(set,runs.values())))
print('##',os.path.basename(D),'forks',forks,'runs',[len(runs[f]) for f in forks])
K=('treeDigest','uniquesDigest','symDigest')
same=0
for n in common:
    ms=[meta(f,n) for f in forks]
    eq=all(all(m[k]==ms[0][k] for k in K) for m in ms)
    same+=eq
    if not eq: print(' run',n,'differs:', ' | '.join('t%s u%s s%s fp%s>%s'%(m['treeDigest'],m['uniquesDigest'],m['symDigest'],m['startFingerprint'],m['endFingerprint']) for m in ms))
print(' fork-to-fork identical runs: %d/%d'%(same,len(common)))
for f in forks:
    ms=[meta(f,n) for n in runs[f]]
    d=sum(1 for a,b in zip(ms,ms[1:]) if a['treeDigest']!=b['treeDigest'])
    print(' drift',f,'runs whose tree digest differs from the previous run: %d/%d'%(d,len(ms)-1), 'trees',ms[0]['trees'],'uniques',ms[0]['uniques'])
