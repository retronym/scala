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

tpl=open('page.tpl').read()
for key,fn in [('SCEN',scen_chart),('CLASSES',class_chart),('SIZES',size_table),('CORPORA',corpora_table),('POS',pos_table)]:
    tpl=tpl.replace('{{'+key+'}}',both(fn))
tpl=tpl.replace('{{ANAT}}',both(lambda m: anatomy(TREE_ROWS,m))).replace('{{POSANAT}}',both(lambda m: anatomy(POS_ROWS,m)))
# inline per-model numbers: {{n:key}} -> two spans
import re
tpl=re.sub(r'\{\{n:(\w+)\}\}', lambda mo: ''.join(f'<span class="h{m}">{num(m,mo.group(1))}</span>' for m in ('8','12')), tpl)
tpl=tpl.replace('{{TREEN}}',f'{sc["treeN"]/1e6:.2f}M').replace('{{WALKN}}',f'{sc["walkN"]/1e6:.2f}M')
assert '{{' not in tpl, re.findall(r'\{\{[^}]*\}\}',tpl)
open('out/tree-footprint.html','w').write(tpl)
