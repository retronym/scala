import os,sys
os.chdir(sys.argv[1])
forks=sorted(p for p in os.listdir('.') if p.isdigit())
def meta(f,n): return dict(l.rstrip('\n').split('\t',1) for l in open('%s/run-%03d.meta'%(f,n)))
n_runs=min(len([x for x in os.listdir(f) if x.endswith('.meta')]) for f in forks)
for n in range(n_runs):
    ph=[[kv.rsplit('=',1) for kv in meta(f,n)['phases'].split()] for f in forks]
    st=[int(meta(f,n)['startFingerprint']) for f in forks]
    prev=None; out=[]
    for i,(k,v) in enumerate(ph[0]):
        d=tuple(int(p[i][1])-int(v) for p in ph)
        if d!=prev: out.append('%s:%s%s'%(k,d, (' [prev %s]'%ph[0][i-1][0]) if i else '')); prev=d
    ds=tuple(s-st[0] for s in st)
    if len(out)>1 or ds!=(0,)*len(forks): print(n,'start',ds,' '.join(out))
