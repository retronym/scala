// The text summary of x-ray recordings, with the page's own analysis (the `xray-analysis` script of index.html).
//   node summary.js <timeline.data.js | run.json>...
const fs = require('fs'), path = require('path'), vm = require('vm');
const html = fs.readFileSync(path.join(__dirname, 'index.html'), 'utf8');
const src = /<script id="xray-analysis">([\s\S]*?)<\/script>/.exec(html)[1];
const ctx = { module: { exports: null } };
vm.runInNewContext(src, ctx);
const XR = ctx.module.exports;
const runs = process.argv.slice(2).flatMap((f) => XR.parseFile(fs.readFileSync(f, 'utf8'))).map(XR.decode);
runs.sort((p, q) => p.corpus.localeCompare(q.corpus) || (p.N - q.N));
console.log(XR.summary(runs));
