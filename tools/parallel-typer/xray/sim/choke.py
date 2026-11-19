#!/usr/bin/env python3
# Fixed splits against adaptive scheduling, by the share of the work in a chain of chokepoint modules.
import random, math, statistics as st
from sim import *
from run import policies
def build(n, share, parts, seed):
    # `parts` big modules in a chain holding `share` of all the work; everything else, a layered wide graph, hangs off the chain's end or its middle
    G0=synth(n,'wide',seed,120,1.6); tot=sum(G0.work)
    if share<=0: return G0
    big=tot*share/(1-share)/parts
    work=[big]*parts+G0.work
    deps=[[i-1] if i else [] for i in range(parts)]
    r=random.Random(seed)
    for ds in G0.deps:
        nd=[d+parts for d in ds]
        if not nd or r.random()<.3: nd.append(r.randrange(parts))
        deps.append(nd)
    return Graph(work,deps)
for corpus in ('compiler','pekko'):
    c=Curves(corpus)
    for b in (8,16):
        print(f'== {corpus}, {b} cores, n=1000, 4-module chain')
        for share in (0,.02,.05,.1,.2,.3,.5):
            rows={}
            for seed in range(5):
                G=build(1000,share,4,seed); W=sum(G.work)
                for name,p in policies(b): rows.setdefault(name,[]).append(W/simulate(G,c,b,p))
                rows.setdefault('adapt',[]).append(W/simulate(G,c,b,('adaptive',)))
                rows.setdefault('adaptP',[]).append(W/simulate(G,c,b,('adaptive',),penalty=True))
            m={k:st.median(v) for k,v in rows.items()}
            fx={k:v for k,v in m.items() if 'x' in k}; best=max(fx,key=fx.get)
            print(f"share {share:4.0%} "+' '.join(f'{k} {v:5.2f}' for k,v in m.items())+f" | best {best} adapt {100*(m['adapt']/fx[best]-1):+5.1f}% pen {100*(m['adaptP']/fx[best]-1):+5.1f}%")
