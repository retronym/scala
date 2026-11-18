import json, html
D=json.load(open('out/data.json'))
SC=['base','A:-id','B:+sym','A+B','C:-outer','A+B+C']
LBL={'base':'Today','A:-id':'A · drop id','B:+sym':'B · symbol on Tree','A+B':'A + B','C:-outer':'C · drop $outer','A+B+C':'A + B + C'}
SHORT={'base':'Today','A:-id':'A','B:+sym':'B','A+B':'A+B','C:-outer':'C','A+B+C':'A+B+C'}
MB=lambda b: b/1048576
sc=D['scala']
MODELS={'8':'8B-hdr','12':'12B-hdr'}
HSLOTS={'8':2,'12':3}
COLOFF={'12':0,'8':6}   # offset into row['sizes']

def both(fn):
    """Render fn(model) for both models, each wrapped in its toggle class."""
    return ''.join(f'<div class="h{m}">{fn(m)}</div>' for m in ('8','12'))

def tree_bytes(m, s, rows=None):
    rows = rows or sc['rows']
    i = COLOFF[m] + SC.index(s)
    return sum(r['heapN'] * r['sizes'][i] for r in rows)

# --- scenario totals, one model
def scen_chart(m):
    tot=sc['tot'][MODELS[m]]; base=tot['base']
    W,rowh,lab=760,30,170; maxv=MB(sc['tot']['12B-hdr']['base'])*1.12
    x=lambda v: lab+(W-lab-130)*v/maxv
    out=[]; y=6
    for s in SC:
        v=MB(tot[s]); d=100*(tot[s]-base)/base
        out.append(f'<text x="{lab-10}" y="{y+16}" class="lab" text-anchor="end">{html.escape(LBL[s])}</text>')
        out.append(f'<rect x="{lab}" y="{y+5}" width="{x(v)-lab:.1f}" height="16" class="{"b12" if s=="base" else "b8"}" rx="2"/>')
        tag = '' if s=='base' else f' <tspan class="{"neg" if d<0 else "pos"}">{d:+.1f}%</tspan>'
        out.append(f'<text x="{x(v)+6:.1f}" y="{y+17}" class="val">{v:.1f} MB{tag}</text>')
        y+=rowh
    ticks=''.join(f'<line x1="{x(t):.1f}" x2="{x(t):.1f}" y1="4" y2="{y}" class="grid"/><text x="{x(t):.1f}" y="{y+14}" class="tick" text-anchor="middle">{t} MB</text>' for t in range(0,int(maxv)+1,10))
    return f'<svg viewBox="0 0 {W} {y+22}" class="chart" role="img" aria-label="Tree bytes per scenario">{ticks}{"".join(out)}</svg>'

# --- top classes by bytes, one model
def class_chart(m):
    i=COLOFF[m]
    rows=sorted(sc['rows'],key=lambda r:-r['heapN']*r['sizes'][i])[:16]
    W,rh,lab=760,22,150; maxv=MB(rows[0]['heapN']*rows[0]['sizes'][i])*1.18
    x=lambda v: lab+(W-lab-160)*v/maxv
    out=[]; y=6
    for r in rows:
        v=MB(r['heapN']*r['sizes'][i])
        out.append(f'<text x="{lab-10}" y="{y+14}" class="lab mono" text-anchor="end">{r["cls"]}</text>')
        out.append(f'<rect x="{lab}" y="{y+3}" width="{x(v)-lab:.1f}" height="14" class="{"sym" if r["sym"] else "nosym"}" rx="2"/>')
        out.append(f'<text x="{x(v)+6:.1f}" y="{y+14}" class="val">{v:.1f} MB · {r["heapN"]/1000:.0f}k × {r["sizes"][i]} B</text>')
        y+=rh
    return f'<svg viewBox="0 0 {W} {y+6}" class="chart" role="img" aria-label="Top tree classes by retained bytes">{"".join(out)}</svg>'

def size_table(m):
    i=COLOFF[m]; total=tree_bytes(m,'base')
    rows=sorted(sc['rows'],key=lambda r:-r['heapN']*r['sizes'][i])[:20]
    h='<tr><th>Class</th><th class="num">Live</th><th class="num">Share</th>'+''.join(f'<th class="num">{SHORT[s]}</th>' for s in SC)+'</tr>'
    body=''
    for r in rows:
        sz=r['sizes'][i:i+6]; b0=sz[0]
        cells=''.join(f'<td class="num {"dn" if s<b0 else "up" if s>b0 else ""}">{s}</td>' for s in sz)
        body+=f'<tr><td class="mono">{r["cls"]}{" <span class=chip>sym</span>" if r["sym"] else ""}</td><td class="num">{r["heapN"]:,}</td><td class="num">{100*r["heapN"]*b0/total:.1f}%</td>{cells}</tr>'
    return f'<table>{h}{body}</table>'

def corpora_table(m):
    h='<tr><th>Corpus</th><th class="num">Trees</th><th class="num">Tree bytes</th>'+''.join(f'<th class="num">{SHORT[s]}</th>' for s in SC[1:])+'</tr>'
    body=''
    for name in ['scala','scalap','re2s']:
        c=D[name]; t=c['tot'][MODELS[m]]; b=t['base']
        cells=''.join(f'<td class="num {"dn" if t[s]<b else "up"}">{100*(t[s]-b)/b:+.1f}%</td>' for s in SC[1:])
        body+=f'<tr><td class="mono">{name}</td><td class="num">{c["treeN"]:,}</td><td class="num">{MB(b):.1f} MB</td>{cells}</tr>'
    return f'<table>{h}{body}</table>'

# position classes: field slot count (4-byte fields), from JOL
POSFIELDS={'RangePosition':4,'OffsetPosition':2,'SingleAttachment':2,'TransparentPosition':4,'NonemptyAttachments':2}
def pos_size(c,m): return (int(m)+4*POSFIELDS[c]+7)&~7
def pos_bytes(m): return sum(n*pos_size(c,m) for c,n,_ in sc['pos'])
def pos_table(m):
    rows=sc['pos']
    return '<table><tr><th>Class</th><th class="num">Live</th><th class="num">Each</th><th class="num">Bytes</th></tr>'+''.join(f'<tr><td class="mono">{c}</td><td class="num">{n:,}</td><td class="num">{pos_size(c,m)} B</td><td class="num">{MB(n*pos_size(c,m)):.1f} MB</td></tr>' for c,n,_ in rows)+f'<tr class="tot"><td>Total</td><td></td><td></td><td class="num">{MB(pos_bytes(m)):.1f} MB</td></tr></table>'

# --- slot diagrams: padding and size are computed from the header and field slots
SLOT=46
def anatomy(rows, m):
    hs=HSLOTS[m]; s=''; y=0
    for title,fields in rows:
        cells=[('header','hdr',hs)]+[(f,'fs' if f=='sym' else 'fid' if f=='id' else 'f',1) for f in fields]
        used=sum(w for *_,w in cells)
        if used%2: cells.append(('pad','pad',1)); used+=1
        s+=f'<text x="0" y="{y+17}" class="lab mono">{title}</text>'; x0=130
        for lab,cls,w in cells:
            s+=f'<rect x="{x0}" y="{y}" width="{w*SLOT-2}" height="26" class="{cls}" rx="2"/><text x="{x0+(w*SLOT-2)/2}" y="{y+17}" class="cell" text-anchor="middle">{lab}</text>'
            x0+=w*SLOT
        s+=f'<text x="{130+10*SLOT+8}" y="{y+17}" class="val">{used*4} B</text>'; y+=36
    return f'<svg viewBox="0 0 {130+10*SLOT+60} {y}" class="chart anat" role="img" aria-label="Field layout in 4-byte slots">{s}</svg>'

BASE=['$outer','id','tpe','att']; BASE_A=['$outer','tpe','att']
TREE_ROWS=[
  ('Ident today',BASE+['sym','name']), ('Ident, A',BASE_A+['sym','name']),
  ('Apply today',BASE+['fun','args']), ('Apply, A',BASE_A+['fun','args']), ('Apply, A+B',BASE_A+['sym','fun','args']),
  ('Select today',BASE+['sym','qual','name']), ('Select, A',BASE_A+['sym','qual','name']),
  ('Literal today',BASE+['value']), ('Literal, A+B',BASE_A+['sym','value']),
]
POS_ROWS=[
  ('OffsetPosition',['source','point']), ('RangePosition',['source','start','point','end']),
  ('TransparentPos.',['source','start','point','end']), ('SingleAttachment',['pos','att']), ('NonemptyAttach.',['pos','all']),
]

def pct(m,s): t=sc['tot'][MODELS[m]]; return f'{100*(t[s]-t["base"])/t["base"]:+.1f}%'.replace('-','−')
def num(m,key):
    t=sc['tot'][MODELS[m]]
    return {'treeMB':f'{MB(t["base"]):.0f}','posMB':f'{MB(pos_bytes(m)):.0f}','A':pct(m,'A:-id'),'AB':pct(m,'A+B'),'B':pct(m,'B:+sym'),
            'C':pct(m,'C:-outer'),'ABC':pct(m,'A+B+C'),'base':f'{4*HSLOTS[m]+16}'}[key]


# ---------------- Symbol, Type, Name and Scope objects ----------------
OB=D['objects']; ob=OB['scala']; OSC=ob['scn']
FAMS=['Symbol','Type','Name','Scope']
FAMCLS={'Symbol':'fsym','Type':'ftype','Name':'fname','Scope':'fscope'}
OLBL={'S1':'drop Symbol.initName','S2':'javaBinaryNameStringCache to ClassSymbol','S3':'inline TypeHistory head into Symbol',
      'S4':'TermSymbol._referenced to ModuleSymbol','S1-4':'S1 to S4 together','T1':'TypeRef parents cache to a side table',
      'T2':'lazy BaseTypeSeq.pending','N1':'drop Name.cachedString','all':'all of the above','X':'drop cake $outer (reference)'}
def short(c):
    c=c.replace('scala.collection.mutable.BitSet','mutable.BitSet')
    return c.split('.')[-1].split('$')[-1] if not c.startswith('mutable') else c
def osz(r,m): return r['s'+m]
def obytes(m, i=0, o=ob):   # family -> bytes for scenario index i
    return {f:o['tot'][m][f][i] for f in FAMS}
def items(m, o=ob):
    """Class rows and owned arrays as (label, fam, n, bytes, per-instance size or None)."""
    xs=[(short(r['cls']),r['fam'],r['n'],r['n']*osz(r,m)[0],osz(r,m)[0]) for r in o['rows']]
    xs+=[(x['name']+' []' if not x['name'].endswith('elems') else x['name']+' []',x['fam'],x['arrays'],x['b'+m][0],None) for x in o['owned']]
    return sorted(xs,key=lambda x:-x[3])

def fam_chart(m):
    t=obytes(m); trees=tree_bytes(m,'base'); pos=pos_bytes(m)
    bars=[('Trees',trees,'b12'),('Positions',pos,'b12')]+[(f+('s' if f!='Type' else 's'),t[f],FAMCLS[f]) for f in FAMS]
    bars=sorted(bars,key=lambda b:-b[1])
    W,rh,lab=760,26,110; maxv=MB(bars[0][1])*1.15
    x=lambda v: lab+(W-lab-110)*v/maxv
    out=[];y=6
    for name,v,cls in bars:
        out.append(f'<text x="{lab-10}" y="{y+15}" class="lab" text-anchor="end">{name}</text><rect x="{lab}" y="{y+4}" width="{x(MB(v))-lab:.1f}" height="15" class="{cls}" rx="2"/><text x="{x(MB(v))+6:.1f}" y="{y+16}" class="val">{MB(v):.1f} MB</text>')
        y+=rh
    return f'<svg viewBox="0 0 {W} {y+4}" class="chart" role="img" aria-label="Bytes by object family">{"".join(out)}</svg>'

def obj_class_chart(m):
    xs=items(m)[:18]
    W,rh,lab=760,22,210; maxv=MB(xs[0][3])*1.2
    x=lambda v: lab+(W-lab-170)*v/maxv
    out=[];y=6
    for name,fam,n,b,sz in xs:
        v=MB(b); tail=f'{n/1000:.0f}k × {sz} B' if sz else f'{n:,} arrays'
        out.append(f'<text x="{lab-10}" y="{y+14}" class="lab mono" text-anchor="end">{html.escape(name)}</text><rect x="{lab}" y="{y+3}" width="{x(v)-lab:.1f}" height="14" class="{FAMCLS[fam]}" rx="2"/><text x="{x(v)+6:.1f}" y="{y+14}" class="val">{v:.1f} MB · {tail}</text>')
        y+=rh
    return f'<svg viewBox="0 0 {W} {y+6}" class="chart" role="img" aria-label="Top Symbol, Type, Name and Scope classes by bytes">{"".join(out)}</svg>'

# field strips: 5 px per byte, colored by how often the field holds a non-default value
LAYCLS=['Symbols$TermSymbol','Symbols$MethodSymbol','Symbols$TypeHistory','Symbols$ClassSymbol','Types$ClassArgsTypeRef','Types$AbstractNoArgsTypeRef',
        'Types$MethodType','Types$RefinedType0','BaseTypeSeqs$BaseTypeSeq','Scopes$Scope','Scopes$ScopeEntry','Names$TermName']
LAYCLS=['scala.reflect.internal.'+c for c in LAYCLS]
def use_of(cls):
    u={x['field']:x for x in ob['use'].get(cls,[])}
    return u
def frac(u,label):
    x=u.get(label)
    return None if x is None else x['nonDefault']/max(x['n'],1)
def ucls(f): return 'uhi' if f is None or f>=0.5 else 'umid' if f>=0.05 else 'ulo'
def strips(m):
    PX=5; s=''; y=0; lab=200
    for cls in LAYCLS:
        lay=ob['layout'][cls][m]; u=use_of(cls); size=next(r for r in ob['rows'] if r['cls']==cls)['s'+m][0]
        s+=f'<text x="{lab-10}" y="{y+16}" class="lab mono" text-anchor="end">{short(cls)}</text>'
        s+=f'<rect x="{lab}" y="{y}" width="{int(m)*PX-1}" height="22" class="hdr" rx="2"><title>header, {m} bytes</title></rect>'
        for off,sz,l in lay:
            if l=='pad': s+=f'<rect x="{lab+off*PX}" y="{y}" width="{sz*PX-1}" height="22" class="pad" rx="2"><title>padding, {sz} bytes at {off}</title></rect>'
            else:
                f=frac(u,l); pct='' if f is None else f', non-default in {100*f:.1f}%'
                s+=f'<rect x="{lab+off*PX}" y="{y}" width="{sz*PX-1}" height="22" class="{ucls(f)}" rx="2"><title>{html.escape(l)}: {sz} bytes at {off}{pct}</title></rect>'
        s+=f'<text x="{lab+size*PX+8}" y="{y+16}" class="val">{size} B</text>'; y+=30
    return f'<svg viewBox="0 0 {lab+150*PX+50} {y}" class="chart" role="img" aria-label="Field layouts, 5 px per byte">{s}</svg>'

def field_tables(m):
    out=''
    for cls in LAYCLS:
        lay=ob['layout'][cls][m]; u=use_of(cls); r=next(r for r in ob['rows'] if r['cls']==cls)
        rows=f'<tr><td class="num">0</td><td class="num">{m}</td><td class="sub">header</td><td></td><td></td></tr>'
        for off,sz,l in lay:
            if l=='pad': rows+=f'<tr class="padrow"><td class="num">{off}</td><td class="num">{sz}</td><td class="sub">padding</td><td></td><td></td></tr>'; continue
            x=u.get(l); f=frac(u,l)
            pct='' if f is None else f'{100*f:.1f}%'
            sen='' if x is None else html.escape(x['sentinels'].replace(',',', '))
            rows+=f'<tr><td class="num">{off}</td><td class="num">{sz}</td><td class="mono">{html.escape(l)}</td><td class="num {ucls(f)}t">{pct}</td><td class="sub">{sen}</td></tr>'
        opn=' open' if short(cls) in ('TermSymbol','ClassArgsTypeRef') else ''
        out+=f'<details{opn}><summary><code>{short(cls)}</code> · {r["s"+m][0]} B · {r["n"]:,} live · {MB(r["n"]*r["s"+m][0]):.1f} MB</summary><table><tr><th class="num">Offset</th><th class="num">Bytes</th><th>Field</th><th class="num">Non-default</th><th>Default values seen</th></tr>{rows}</table></details>'
    return out

def cand_table(m):
    base=sum(obytes(m).values())
    h='<tr><th>Candidate</th><th class="num">Δ MB</th><th class="num">Δ of these objects</th><th class="num">Δ of live heap</th></tr>'
    body=''
    for i,s in enumerate(OSC[1:],1):
        d=sum(obytes(m,i).values())-base
        body+=f'<tr{" class=tot" if s=="all" else ""}><td><b>{s}</b> <span class="sub">{html.escape(OLBL[s])}</span></td><td class="num {"dn" if d<0 else ""}">{MB(d):+.2f}</td><td class="num {"dn" if d<0 else ""}">{100*d/base:+.1f}%</td><td class="num">{100*d/(ob["heapMB"]*1048576):+.1f}%</td></tr>'
    return f'<table>{h}{body}</table>'

def obj_corpora(m):
    h='<tr><th>Corpus</th><th class="num">These objects</th>'+''.join(f'<th class="num">{s}</th>' for s in OSC[1:])+'</tr>'
    body=''
    for name in ['scala','scalap','re2s']:
        o=OB[name]; b=sum(obytes(m,0,o).values())
        cells=''.join((lambda d:f'<td class="num {"dn" if d<0 else ""}">{100*d/b:+.1f}%</td>')(sum(obytes(m,i,o).values())-b) for i in range(1,len(OSC)))
        body+=f'<tr><td class="mono">{name}</td><td class="num">{MB(b):.1f} MB</td>{cells}</tr>'
    return f'<table>{h}{body}</table>'

def obj_sizes(m):
    rows=sorted(ob['rows'],key=lambda r:-r['n']*r['s'+m][0])[:16]
    h='<tr><th>Class</th><th class="num">Live</th>'+''.join(f'<th class="num">{s}</th>' for s in OSC)+'</tr>'
    body=''
    for r in rows:
        sz=r['s'+m]; b0=sz[0]
        cells=''.join(f'<td class="num {"dn" if v<b0 else "up" if v>b0 else ""}">{v if v else "–"}</td>' for v in sz)
        body+=f'<tr><td class="mono">{short(r["cls"])}</td><td class="num">{r["n"]:,}</td>{cells}</tr>'
    return f'<table>{h}{body}</table>'

def padding_table(m):
    """Classes where any one 4-byte field could go for free (>= 4 bytes of padding), weighted by live count."""
    xs=[]
    for r in ob['rows']:
        lay=ob['layout'].get(r['cls'],{}).get(m)
        if not lay: continue
        pad=sum(sz for _,sz,l in lay if l=='pad')
        xs.append((r,pad))
    good=sorted([x for x in xs if x[1]>=4],key=lambda x:-x[0]['n'])[:12]
    full=sorted([x for x in xs if x[1]==0],key=lambda x:-x[0]['n'])[:12]
    def tbl(ys,col,f):
        return '<table><tr><th>Class</th><th class="num">Size</th><th class="num">Padding</th><th class="num">Live</th><th class="num">'+col+'</th></tr>'+''.join(
            f'<tr><td class="mono">{short(r["cls"])}</td><td class="num">{r["s"+m][0]} B</td><td class="num">{p} B</td><td class="num">{r["n"]:,}</td><td class="num">{f(r)}</td></tr>' for r,p in ys)+'</table>'
    return ('<p class="note">Removing any one 4-byte field saves 8 bytes per instance:</p><div class="box">'+tbl(good,'Saving',lambda r:f'{MB(8*r["n"]):.2f} MB')+
            '</div><p class="note">No padding, so adding a 4-byte field costs 8 bytes per instance:</p><div class="box">'+tbl(full,'Cost',lambda r:f'{MB(8*r["n"]):.2f} MB')+'</div>')

def onum(m,key):
    t=obytes(m); base=sum(t.values())
    if key in FAMS: return f'{MB(t[key]):.0f}'
    if key=='objMB': return f'{MB(base):.0f}'
    if key=='objPct': return f'{100*base/(ob["heapMB"]*1048576):.0f}'
    i=OSC.index(key); d=sum(obytes(m,i).values())-base
    return f'{MB(d):+.1f} MB'.replace('-','−')

tpl=open('page.tpl').read()
for key,fn in [('SCEN',scen_chart),('CLASSES',class_chart),('SIZES',size_table),('CORPORA',corpora_table),('POS',pos_table),
               ('FAMCHART',fam_chart),('OBJCLASSES',obj_class_chart),('STRIPS',strips),('FIELDS',field_tables),('CANDS',cand_table),
               ('OBJCORPORA',obj_corpora),('OBJSIZES',obj_sizes),('PADDING',padding_table)]:
    tpl=tpl.replace('{{'+key+'}}',both(fn))
tpl=tpl.replace('{{ANAT}}',both(lambda m: anatomy(TREE_ROWS,m))).replace('{{POSANAT}}',both(lambda m: anatomy(POS_ROWS,m)))
# inline per-model numbers: {{n:key}} -> two spans
import re
tpl=re.sub(r'\{\{n:(\w+)\}\}', lambda mo: ''.join(f'<span class="h{m}">{num(m,mo.group(1))}</span>' for m in ('8','12')), tpl)
tpl=re.sub(r'\{\{o:([\w-]+)\}\}', lambda mo: ''.join(f'<span class="h{m}">{onum(m,mo.group(1))}</span>' for m in ('8','12')), tpl)
tpl=tpl.replace('{{NAMECHARS}}',f'{ob["nameTableSize"]:,}')
tpl=tpl.replace('{{TREEN}}',f'{sc["treeN"]/1e6:.2f}M').replace('{{WALKN}}',f'{sc["walkN"]/1e6:.2f}M')
assert '{{' not in tpl, re.findall(r'\{\{[^}]*\}\}',tpl)
open('out/tree-footprint.html','w').write(tpl)
