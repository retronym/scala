<title>Scalac Tree Footprint</title>
<meta name="claude:viewport-fit-sides" content="cover">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600&family=IBM+Plex+Mono:wght@400;500&family=Space+Grotesk:wght@500;600&display=swap">
<style>
/* Layout: sticky header-model bar, then a single reading column (~880px); charts full column width, tables scroll in their own box */
:root{
  --bg:#f6f7f9; --panel:#ffffff; --fg:#1b2330; --muted:#5d6878; --rule:#dde2ea;
  --accent:#2f5fd0; --accent2:#8aa6e8; --sym:#2f5fd0; --nosym:#c27a2c;
  --good:#1f8a5b; --bad:#c2412d; --hdr:#c9d0db; --fid:#e8b4a8; --pad:#eef0f3; --padline:#b9c1cd; --bar:rgba(246,247,249,.92);
  --display:"Space Grotesk",system-ui,sans-serif; --body:"IBM Plex Sans",system-ui,sans-serif; --mono:"IBM Plex Mono",ui-monospace,Menlo,monospace;
}
@media (prefers-color-scheme: dark){:root:not([data-theme="light"]){
  --bg:#12161d; --panel:#1a2029; --fg:#e4e8ef; --muted:#97a2b3; --rule:#2c3440;
  --accent:#7ea2ff; --accent2:#3d5594; --sym:#7ea2ff; --nosym:#e0a25e;
  --good:#5fcf98; --bad:#ff8a73; --hdr:#3a4352; --fid:#8a4a3e; --pad:#20262f; --padline:#4a5566; --bar:rgba(18,22,29,.92); color-scheme:dark}}
:root[data-theme="dark"]{
  --bg:#12161d; --panel:#1a2029; --fg:#e4e8ef; --muted:#97a2b3; --rule:#2c3440;
  --accent:#7ea2ff; --accent2:#3d5594; --sym:#7ea2ff; --nosym:#e0a25e;
  --good:#5fcf98; --bad:#ff8a73; --hdr:#3a4352; --fid:#8a4a3e; --pad:#20262f; --padline:#4a5566; --bar:rgba(18,22,29,.92); color-scheme:dark}
:root:not([data-hdr="12"]) .h12{display:none}
:root[data-hdr="12"] .h8{display:none}
body{background:var(--bg);color:var(--fg);font:15px/1.55 var(--body)}
body::before{content:"";position:fixed;inset:0 0 auto;height:env(safe-area-inset-top,0px);z-index:10;background:var(--bar)}
.topbar{position:sticky;top:env(safe-area-inset-top,0px);z-index:5;background:var(--bar);backdrop-filter:blur(6px);border-bottom:1px solid var(--rule)}
.topbar .in{max-width:880px;margin:0 auto;padding-block:10px;padding-left:max(16px,env(safe-area-inset-left,0px));padding-right:max(16px,env(safe-area-inset-right,0px));display:flex;flex-wrap:wrap;align-items:center;gap:8px 16px}
.topbar .name{font:600 15px var(--display);margin-right:auto}
.seg{display:inline-flex;border:1px solid var(--rule);border-radius:6px;overflow:hidden;background:var(--panel)}
.seg button{font:500 13px var(--body);color:var(--muted);background:transparent;border:0;padding:6px 12px;cursor:pointer}
.seg button+button{border-left:1px solid var(--rule)}
.seg button[aria-pressed="true"]{background:var(--accent);color:var(--bg)}
.seg button:focus-visible{outline:2px solid var(--accent);outline-offset:-2px}
.seglab{font-size:12px;color:var(--muted);letter-spacing:.04em;text-transform:uppercase}
main{max-width:880px;margin:0 auto;padding-block:28px 64px;padding-left:max(16px,env(safe-area-inset-left,0px));padding-right:max(16px,env(safe-area-inset-right,0px));display:grid;gap:28px}
h1,h2{font-family:var(--display);text-wrap:balance;margin:0}
h1{font-size:30px;line-height:1.15}
h2{font-size:19px}
p{margin:0;max-width:68ch}
.lede{color:var(--muted)}
section{display:grid;gap:12px;min-width:0}
.stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:12px}
.stat{background:var(--panel);border:1px solid var(--rule);border-radius:6px;padding:12px 14px}
.stat b{display:block;font:600 24px var(--display);font-variant-numeric:tabular-nums}
.stat span{color:var(--muted);font-size:13px}
.box{overflow-x:auto;background:var(--panel);border:1px solid var(--rule);border-radius:6px;padding:12px}
.chart{width:100%;min-width:560px;height:auto;display:block}
.anat{max-width:720px}
.chart text{fill:var(--fg);font:12px var(--body)}
.chart .lab{font-size:12.5px}.chart .mono{font-family:var(--mono);font-size:12px}
.chart .val{fill:var(--muted);font-variant-numeric:tabular-nums}
.chart .tick{fill:var(--muted);font-size:11px}
.chart .grid{stroke:var(--rule)}
.chart .neg{fill:var(--good);font-weight:600}.chart .pos{fill:var(--bad);font-weight:600}
.b12{fill:var(--muted)}.b8{fill:var(--accent)}.sym{fill:var(--sym)}.nosym{fill:var(--nosym)}
.hdr{fill:var(--hdr)}.f{fill:var(--accent2)}.fs{fill:var(--sym)}.fid{fill:var(--fid)}.pad{fill:var(--pad);stroke:var(--padline);stroke-dasharray:3 2}
.chart .cell{font:11px var(--mono);fill:var(--fg)}
.chart .fs+.cell{fill:var(--bg)}
.legend{display:flex;flex-wrap:wrap;gap:14px;font-size:13px;color:var(--muted)}
.legend i{display:inline-block;width:11px;height:11px;border-radius:2px;margin-right:6px;vertical-align:-1px}
table{border-collapse:collapse;width:100%;font-size:13px;font-variant-numeric:tabular-nums}
th,td{padding:5px 8px;border-bottom:1px solid var(--rule);text-align:left;white-space:nowrap}
th{font-weight:600;color:var(--muted);font-size:12px;vertical-align:bottom}
.num{text-align:right}.mono{font-family:var(--mono);font-size:12.5px}.sub{color:var(--muted);font-weight:400;font-size:11px}
td.dn{color:var(--good);font-weight:600}td.up{color:var(--bad);font-weight:600}
tr.tot td{font-weight:600;border-bottom:none}
.chip{font:500 10px var(--body);letter-spacing:.04em;text-transform:uppercase;color:var(--sym);border:1px solid currentColor;border-radius:3px;padding:0 4px;margin-left:4px}
ul{margin:0;padding-left:20px;max-width:68ch;display:grid;gap:6px}
code{font-family:var(--mono);font-size:13px}
.note{font-size:13px;color:var(--muted)}
</style>

<div class="topbar"><div class="in">
<span class="name">Scalac tree footprint</span>
<span class="seglab" id="seglab">Object header</span>
<div class="seg" role="group" aria-labelledby="seglab">
<button type="button" id="hdr8" data-hdr="8" aria-pressed="true">Compact · 8 B</button>
<button type="button" id="hdr12" data-hdr="12" aria-pressed="false">Legacy · 12 B</button>
</div>
</div></div>

<main>
<header style="display:grid;gap:10px">
<h1>Scalac tree footprint after typer</h1>
<p class="lede">Live <code>Tree</code> and <code>Position</code> instances once typer finishes, measured on the compiler-benchmark corpora with scalac 2.13.18 on JDK 25, and what three candidate field changes do to the weighted total. <span class="h8">Figures assume compact object headers (<code>-XX:+UseCompactObjectHeaders</code>, 8-byte header). Counts are measured; sizes are modelled from the same field layout that reproduces the measured 12-byte sizes exactly.</span><span class="h12">Figures use today's default 12-byte header, which the run measured directly; the modelled size of every tree class matches the heap histogram.</span></p>
</header>

<div class="stats">
<div class="stat"><b>{{TREEN}}</b><span>live trees, scala corpus (1135 files)</span></div>
<div class="stat"><b>{{n:treeMB}} MB</b><span>tree bytes, scala corpus</span></div>
<div class="stat"><b>{{n:posMB}} MB</b><span>positions and attachments</span></div>
<div class="stat"><b>{{n:A}} / {{n:AB}}</b><span>tree bytes from A (drop <code>id</code>) / A+B</span></div>
</div>

<section>
<h2>Every tree pays {{n:base}} bytes before its own fields</h2>
<p>The base is the object header plus four 4-byte fields: <code>$outer</code> (in <code>TreeContextApiImpl</code>, the cake's <code>SymbolTable</code>), <code>id</code>, <code>rawtpe</code> and <code>rawatt</code>. Objects round up to 8 bytes, so a field only saves memory when it removes a padding slot, and only costs memory when it needs a new one. <span class="h8">With the 8-byte header, a class with an even number of fields has no padding.</span><span class="h12">With the 12-byte header, a class with an odd number of fields has no padding.</span></p>
<div class="box">{{ANAT}}</div>
<div class="legend"><span><i style="background:var(--hdr)"></i>header</span><span><i style="background:var(--fid)"></i>id</span><span><i style="background:var(--sym)"></i>symbol</span><span><i style="background:var(--accent2)"></i>other field</span><span><i style="background:var(--pad);outline:1px dashed var(--padline)"></i>alignment padding</span></div>
</section>

<section>
<h2>The three candidates, scala corpus</h2>
<p>Live tree bytes under each change, against today's layout with the same header.</p>
<div class="box">{{SCEN}}</div>
<ul class="h8">
<li><b>A, drop <code>id</code>:</b> {{n:A}}. Classes with an odd field count lose their padding and shrink by 8: Select 40→32, ValDef 48→40, DefDef 56→48, Literal and New 32→24, CaseDef, If, Bind, Function. Ident, TypeTree, Apply and This are already full at 32 and gain nothing.</li>
<li><b>B, <code>symbol</code> on every tree:</b> {{n:B}}. SymTrees are unchanged. Non-SymTrees with an even field count need a new slot: TypeTree, Apply, TypeApply, Block and AppliedTypeTree grow 32→40. Literal, New, CaseDef and If take the field into their padding.</li>
<li><b>A + B:</b> {{n:AB}}. B gives back only 1.4 points of A's saving. TypeTree, Apply and Select all end at 32; the classes that lose A's saving are the odd-count non-SymTrees such as Literal (back to 32) and CaseDef (back to 40).</li>
<li><b>C, drop <code>$outer</code></b> (reference only): {{n:C}}, the same slot effect as A. A+B+C reaches {{n:ABC}}. Tree methods reach the universe through the outer, so this would need them to get it another way.</li>
</ul>
<ul class="h12">
<li><b>A, drop <code>id</code>:</b> {{n:A}}. Every class with an even field count (TypeTree, Apply, Ident, This, TypeApply, Block, …) loses its padding and shrinks 40→32. Select, ValDef, DefDef and Literal already fill their last slot and gain nothing.</li>
<li><b>B, <code>symbol</code> on every tree:</b> {{n:B}}. SymTrees are unchanged; the non-SymTrees with a padding slot take the field for free, and Literal, New, CaseDef, If and Throw grow by 8.</li>
<li><b>A + B:</b> {{n:AB}}. B uses the slot that A frees: against A alone it gives back 6.2 points, since TypeTree and Apply return to 40.</li>
<li><b>C, drop <code>$outer</code></b> (reference only): {{n:C}}, the same slot effect as A. A+B+C reaches {{n:ABC}}. Tree methods reach the universe through the outer, so this would need them to get it another way.</li>
</ul>
</section>

<section>
<h2>Same picture on three corpora</h2>
<p>Change in live tree bytes against today's layout with the same header. The smaller corpora have more TypeTree and Apply per Select, so B's cost moves a little, but the ranking holds.</p>
<div class="box">{{CORPORA}}</div>
</section>

<section>
<h2>Where the bytes are</h2>
<p>The top classes by retained bytes in the scala corpus. Select and Ident make up about 40% of tree bytes; TypeTree, mostly inferred types, is next.</p>
<div class="box">{{CLASSES}}</div>
<div class="legend"><span><i style="background:var(--sym)"></i>SymTree subclass (has a <code>symbol</code> field today)</span><span><i style="background:var(--nosym)"></i>other tree</span></div>
<p class="note">Instance size per scenario. Green is smaller than today's layout with the same header, red larger.</p>
<div class="box">{{SIZES}}</div>
</section>

<section>
<h2>Positions</h2>
<p>Positions take about half as many bytes as trees. <code>RangePosition</code> holds a source reference plus start, point and end; <code>OffsetPosition</code> a source and point. Trees often share a position object (copyAttrs, focus), so there are fewer positions than trees. <code>SingleAttachment</code> wraps a position when a tree carries an attachment.</p>
<div class="box">{{POSANAT}}</div>
<div class="legend"><span><i style="background:var(--hdr)"></i>header</span><span><i style="background:var(--accent2)"></i>field</span><span><i style="background:var(--pad);outline:1px dashed var(--padline)"></i>alignment padding</span></div>
<p class="h8">With compact headers every position and attachment class has an even field count, so none carries padding: OffsetPosition is 16 B and RangePosition 24 B. A field added to a position would cost 8 bytes; removing one would save nothing. <code>TransparentPosition</code> adds no fields to <code>RangePosition</code>.</p>
<p class="h12">With 12-byte headers every position and attachment class carries 4 bytes of padding: OffsetPosition is 24 B and RangePosition 32 B. A field added to a position would cost nothing; removing one would save 8 bytes. Compact headers remove the padding and save 8 bytes on each, about 8.8 MB on the scala corpus. <code>TransparentPosition</code> adds no fields to <code>RangePosition</code>.</p>
<div class="box">{{POS}}</div>
</section>

<section>
<h2>Notes for B beyond footprint</h2>
<ul>
<li>Non-SymTrees with a <code>symbol</code> override delegate rather than store: <code>Apply</code>/<code>TypeApply</code> → <code>fun.symbol</code>, <code>Super</code> → <code>qual.symbol</code>, <code>AppliedTypeTree</code> → <code>tpt.symbol</code>, <code>TypeTree</code> → <code>typeTreeSymbol</code>, <code>ReferenceToBoxed</code>, and nsc's <code>DocDef</code>. A field read in <code>Tree</code> only replaces the virtual call if these cache the delegate's symbol (setters already write through, but <code>TypeTree</code>'s depends on <code>tpe</code>) or keep an override, which leaves the call site polymorphic.</li>
<li>Apply and TypeApply are 11.7% of tree count, and both are among the classes B grows when no padding slot is free.</li>
<li><code>id</code> readers are diagnostics only: position validation errors (<code>Positions.scala</code>), <code>TreeBrowsers</code>, and the tree printer's enclosing-tree prefix. An identity map built when the error is reported serves all of them.</li>
</ul>
</section>

<section>
<h2>Method</h2>
<ul>
<li>Compile each corpus with <code>-Ystop-after:typer</code> in one JVM, holding the <code>Run</code>, then take a live class histogram (<code>GC.class_histogram</code>, after full GC). Corpora: compiler-benchmark <code>scala</code> (library, reflect and compiler, 1135 files, library on the sourcepath), <code>scalap</code>, <code>re2s</code>. All compile without errors. The live heap after typer on the scala corpus was 334 MB with 12-byte headers, trees 19.7% of it and positions 9.7%.</li>
<li>The heap counts all live trees: {{TREEN}} for scala. An identity-deduplicated walk of unit bodies (including <code>TypeTree.original</code>) reaches {{WALKN}}; the rest are held outside unit bodies, for example by symbols' completers and annotations. Weighting by the walk instead gives the same percentages within 0.3 points.</li>
<li>Sizes per scenario: JOL 0.17 field list per class, 4-byte compressed references, header 12 or 8, rounded up to 8. With only 4-byte and 1-byte fields this rule is exact under JDK 15+ field layout, and with the 12-byte header it matched the histogram's bytes per instance for every tree class.</li>
</ul>
</section>
</main>

<script>
(function(){
  var root=document.documentElement, btns=document.querySelectorAll('.seg button');
  function set(h){
    if(h==='12') root.setAttribute('data-hdr','12'); else root.removeAttribute('data-hdr');
    btns.forEach(function(b){b.setAttribute('aria-pressed', String(b.getAttribute('data-hdr')===(h==='12'?'12':'8')));});
    try{localStorage.setItem('hdr',h);}catch(e){}
  }
  var saved=null; try{saved=localStorage.getItem('hdr');}catch(e){}
  set(saved==='12'?'12':'8');
  btns.forEach(function(b){b.addEventListener('click',function(){set(b.getAttribute('data-hdr'));});});
})();
</script>
