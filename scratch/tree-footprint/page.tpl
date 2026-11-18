<title>Scalac Heap Footprint</title>
<meta name="claude:viewport-fit-sides" content="cover">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600&family=IBM+Plex+Mono:wght@400;500&family=Space+Grotesk:wght@500;600&display=swap">
<style>
/* Layout: sticky header-model bar, then a single reading column (~880px); charts full column width, tables scroll in their own box */
:root{
  --bg:#f6f7f9; --panel:#ffffff; --fg:#1b2330; --muted:#5d6878; --rule:#dde2ea;
  --accent:#2f5fd0; --accent2:#8aa6e8; --sym:#2f5fd0; --nosym:#c27a2c;
  --good:#1f8a5b; --bad:#c2412d; --hdr:#c9d0db; --fid:#e8b4a8; --pad:#eef0f3; --padline:#b9c1cd; --bar:rgba(246,247,249,.92);
  --fsym:#2f5fd0; --ftype:#c27a2c; --fname:#1f8a5b; --fscope:#7a52b3; --uhi:#2f5fd0; --umid:#8aa6e8; --ulo:#e0895f;
  --display:"Space Grotesk",system-ui,sans-serif; --body:"IBM Plex Sans",system-ui,sans-serif; --mono:"IBM Plex Mono",ui-monospace,Menlo,monospace;
}
@media (prefers-color-scheme: dark){:root:not([data-theme="light"]){
  --bg:#12161d; --panel:#1a2029; --fg:#e4e8ef; --muted:#97a2b3; --rule:#2c3440;
  --accent:#7ea2ff; --accent2:#3d5594; --sym:#7ea2ff; --nosym:#e0a25e;
  --good:#5fcf98; --bad:#ff8a73; --hdr:#3a4352; --fid:#8a4a3e; --pad:#20262f; --padline:#4a5566; --bar:rgba(18,22,29,.92); --fsym:#7ea2ff; --ftype:#e0a25e; --fname:#5fcf98; --fscope:#b493e8; --uhi:#7ea2ff; --umid:#3d5594; --ulo:#d9774f; color-scheme:dark}}
:root[data-theme="dark"]{
  --bg:#12161d; --panel:#1a2029; --fg:#e4e8ef; --muted:#97a2b3; --rule:#2c3440;
  --accent:#7ea2ff; --accent2:#3d5594; --sym:#7ea2ff; --nosym:#e0a25e;
  --good:#5fcf98; --bad:#ff8a73; --hdr:#3a4352; --fid:#8a4a3e; --pad:#20262f; --padline:#4a5566; --bar:rgba(18,22,29,.92); --fsym:#7ea2ff; --ftype:#e0a25e; --fname:#5fcf98; --fscope:#b493e8; --uhi:#7ea2ff; --umid:#3d5594; --ulo:#d9774f; color-scheme:dark}
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
.fsym{fill:var(--fsym)}.ftype{fill:var(--ftype)}.fname{fill:var(--fname)}.fscope{fill:var(--fscope)}
.uhi{fill:var(--uhi)}.umid{fill:var(--umid)}.ulo{fill:var(--ulo)}
td.ulot{color:var(--bad);font-weight:600}
.fields{display:grid;gap:6px}
details{background:var(--panel);border:1px solid var(--rule);border-radius:6px;padding:8px 12px;overflow-x:auto}
summary{cursor:pointer;font-size:14px}
tr.padrow td{background:var(--pad)}
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
<span class="name">Scalac heap footprint</span>
<span class="seglab" id="seglab">Object header</span>
<div class="seg" role="group" aria-labelledby="seglab">
<button type="button" id="hdr8" data-hdr="8" aria-pressed="true">Compact · 8 B</button>
<button type="button" id="hdr12" data-hdr="12" aria-pressed="false">Legacy · 12 B</button>
</div>
</div></div>

<main>
<header style="display:grid;gap:10px">
<h1>Scalac heap footprint after typer</h1>
<p class="lede">Live <code>Tree</code>, <code>Position</code>, <code>Symbol</code>, <code>Type</code> and <code>Name</code> instances once typer finishes, measured on the compiler-benchmark corpora with scalac 2.13.18 on JDK 25, and what candidate field changes do to the weighted totals. <span class="h8">Figures assume compact object headers (<code>-XX:+UseCompactObjectHeaders</code>, 8-byte header).</span><span class="h12">Figures use today's default 12-byte header.</span> Counts come from the live heap histogram. Sizes come from a model of HotSpot's field layout, which matches the histogram for every class with both header sizes.</p>
</header>

<div class="stats">
<div class="stat"><b>{{TREEN}}</b><span>live trees, scala corpus (1135 files)</span></div>
<div class="stat"><b>{{n:treeMB}} MB</b><span>tree bytes, scala corpus</span></div>
<div class="stat"><b>{{n:posMB}} MB</b><span>positions and attachments</span></div>
<div class="stat"><b>{{n:A}} / {{n:AB}}</b><span>tree bytes from A (drop <code>id</code>) / A+B</span></div>
<div class="stat"><b>{{o:objMB}} MB</b><span>symbols, types, names and scopes</span></div>
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
<h2>Symbols, types, names and scopes</h2>
<p>Next to trees and positions, the scala corpus holds {{o:objMB}} MB of <code>Symbol</code>, <code>Type</code>, <code>Name</code> and <code>Scope</code> objects after typer, {{o:objPct}}% of the live heap. Each family below includes the objects and arrays it owns: a symbol's <code>TypeHistory</code>; a base type sequence's <code>elems</code> and <code>typeSymbols</code> arrays and its <code>pending</code> bit set; the name table's character array and hash tables; a scope's hash table.</p>
<div class="box">{{FAMCHART}}</div>
<div class="legend"><span><i style="background:var(--fsym)"></i>Symbol</span><span><i style="background:var(--ftype)"></i>Type</span><span><i style="background:var(--fname)"></i>Name</span><span><i style="background:var(--fscope)"></i>Scope</span><span><i style="background:var(--muted)"></i>tree and position, from above</span></div>
<p>The top classes and owned arrays by bytes. A <code>TypeRef</code> with arguments is the largest single class. Every symbol has its own <code>TypeHistory</code>, which makes it the third largest class.</p>
<div class="box">{{OBJCLASSES}}</div>
</section>

<section>
<h2>Field layouts</h2>
<p>Each bar is one instance, 5 px per byte, laid out as HotSpot places the fields: primitives first, largest first, then references, each class's fields filling gaps left by its superclasses. Color shows how often a field holds something other than its default (null, zero, or a sentinel such as <code>NoSymbol</code>, <code>Nil</code> or <code>NoPrefix</code>), counted by a reflective walk of everything reachable from <code>Global</code>. Hover a cell for the field name.</p>
<div class="box">{{STRIPS}}</div>
<div class="legend"><span><i style="background:var(--hdr)"></i>header</span><span><i style="background:var(--uhi)"></i>non-default in ≥ 50%</span><span><i style="background:var(--umid)"></i>5–50%</span><span><i style="background:var(--ulo)"></i>under 5%</span><span><i style="background:var(--pad);outline:1px dashed var(--padline)"></i>alignment padding</span></div>
<div class="fields">{{FIELDS}}</div>
</section>

<section>
<h2>Cheap wins in symbols, types and names</h2>
<p>Each candidate below removes a field that only diagnostics or a rare path reads, or that holds its default in nearly every instance, or it removes an object. None is implemented. Change in bytes of symbols, types, names and scopes, scala corpus:</p>
<div class="box">{{CANDS}}</div>
<ul>
<li><b>S1, drop <code>Symbol.initName</code>.</b> The constructor parameter is kept as a field because <code>sealedSortName</code> reads it. That method orders sealed children for the pickler and for exhaustivity checking. Both run before names are expanded, so <code>rawname</code> may give the same order. This still needs checking.</li>
<li><b>S2, move <code>javaBinaryNameStringCache</code> to <code>ClassSymbol</code>.</b> No symbol has it set after typer. It is read for classes and modules, by the pickler's <code>.sig</code> writer and by the backend, and a module could compute it from its module class.</li>
<li><b>S3, inline the head of the type history.</b> Every symbol owns one <code>TypeHistory</code>, and none has a <code>_prev</code> after typer. Storing the info, its valid-from period and the previous entry on the symbol adds 8 bytes to every symbol and removes a <span class="h8">24</span><span class="h12">32</span> byte object. Later phases add history entries, which would still be separate objects.</li>
<li><b>S4, move <code>TermSymbol._referenced</code> to <code>ModuleSymbol</code>.</b> Every module sets it, to its module class. Only 0.1% of other term symbols and 2.8% of method symbols set it after typer, mostly default getters. Later phases set it on accessors and outer fields, which could use an attachment.</li>
<li><b>T1, move the <code>TypeRef</code> parents cache to a side table.</b> <code>parentsCache</code> and <code>parentsPeriod</code> are set on 0.2% of <code>ClassArgsTypeRef</code> and 1.7% of <code>ClassNoArgsTypeRef</code>. Removing both saves 8 bytes on every <code>TypeRef</code> with either header.</li>
<li><b>T2, allocate <code>BaseTypeSeq.pending</code> lazily.</b> Every base type sequence allocates a <code>BitSet</code> and its <code>long[]</code>. These are only used while a lazy intersection element is being computed. The figure assumes none is needed, so it is an upper bound.</li>
<li><b>N1, drop <code>Name.cachedString</code>.</b> Set on 2.5% of names. It only saves memory with compact headers, and names are small: 53k names in 1.7 MB.</li>
<li><b>X, drop the cake <code>$outer</code></b> from symbols, types, names and type histories, for reference only, like C for trees.</li>
</ul>
<p class="note">Instance size per candidate. Green is smaller than today's layout with the same header, red larger; – means the object is gone.</p>
<div class="box">{{OBJSIZES}}</div>
<p>The same candidates on three corpora. The smaller corpora load most of their symbols from the classpath, so S3 matters more there and the type candidates less.</p>
<div class="box">{{OBJCORPORA}}</div>
</section>

<section>
<h2>Where one field makes a difference</h2>
<p><span class="h8">With compact headers, <code>TermSymbol</code> and <code>MethodSymbol</code> carry 4 bytes of padding, so S1, S2 or S4 alone shrinks them by 8 bytes. Type symbols and modules have none.</span><span class="h12">With 12-byte headers, <code>TermSymbol</code> and <code>MethodSymbol</code> have no padding, so a single dropped field saves nothing on them, and S1 or S2 alone only helps type symbols and modules.</span> <code>ClassArgsTypeRef</code> holds ten 4-byte fields and a 1-byte <code>trivial</code> flag. <span class="h8">With compact headers the flag is the one byte that pushes it from 48 to 56.</span><span class="h12">With 12-byte headers it has 3 bytes of padding, so only dropping two fields (T1) shrinks it.</span></p>
{{PADDING}}
</section>

<section>
<h2>Name table</h2>
<p>A <code>Name</code> is an offset and length into one shared <code>char[]</code>, plus a hash chain link, a cached string and the cake <code>$outer</code>. The table holds {{NAMECHARS}} characters in a 1M-character array, which has doubled from its initial 128k. Names and the table together take {{o:Name}} MB, so no change here is worth much.</p>
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
<li>Tree sizes per scenario: JOL 0.17 field list per class, 4-byte compressed references, header 12 or 8, rounded up to 8. With only 4-byte and 1-byte fields this rule is exact under JDK 15+ field layout.</li>
<li>Symbol, type, name and scope sizes come from a model of HotSpot's JDK 15+ field layout, applied class by class from <code>Object</code> down. Primitives are placed largest first, then references. Each field goes into the smallest gap that fits it at its natural alignment, left by the header or a superclass, or else at the end. This matters for the 8-byte <code>_rawflags</code> and for 1-byte flags.</li>
<li>Each corpus was also run with <code>-XX:+UseCompactObjectHeaders</code>. The modelled sizes match the histogram's bytes per instance for every class, trees included, with both header sizes.</li>
<li>Field use comes from a reflective, identity-deduplicated walk of everything reachable from <code>Global</code>, run after the histogram. Its instance counts match the histogram for every symbol, type, name and scope class. Array bytes are summed per owning field, and for the type and scope arrays they match the histogram.</li>
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
