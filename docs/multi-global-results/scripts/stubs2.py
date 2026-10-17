import sys, collections
for f in sys.argv[1:]:
    tot=0; kinds=collections.Counter(); recv=collections.defaultdict(collections.Counter); caller=collections.defaultdict(collections.Counter)
    for l in open(f):
        st,n=l.rstrip().rsplit(' ',1); n=int(n); tot+=n
        fr=st.split(';')
        for i,x in enumerate(fr):
            if x in ('itable stub','vtable stub'):
                kinds[x]+=n
                if i+1<len(fr): recv[x][fr[i+1]]+=n
                if i>0: caller[x][fr[i-1]]+=n
                break
    pc=lambda v: f"{100*v/tot:5.2f}%"
    print(f"===== {f.split('/')[-1]}")
    for k in ('itable stub','vtable stub'):
        print(f"-- {k}: {pc(kinds[k])}  receivers:")
        for r,v in recv[k].most_common(6): print("   ", pc(v), r)
        print("   callers:")
        for r,v in caller[k].most_common(6): print("   ", pc(v), r)
