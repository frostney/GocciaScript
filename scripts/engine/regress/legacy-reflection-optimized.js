// a non-strict function's caller and arguments read while it runs in optimized code: inlined into a hot loop (one and
// two levels, a strict frame among them, a constructor, recursion), its parameters changed there, the reads repeated
// at one site (an accessor IC hit), and the first read of the program among them
var got = [];
function args(f) { var a = f.arguments; return a === null ? null : Array.prototype.slice.call(a); }
function callerName(f) { var c = f.caller; return c === null ? null : c.name; }
function note(tag, f) { got.push(tag + ' ' + JSON.stringify([args(f), callerName(f)])); }

var o1 = { mid: function mid(x) { x = x + 1; return leaf1(x); } };
function leaf1(x) { if (x === 3001) { note('one', o1.mid); note('one-again', o1.mid); } return x & 1; }
function loop1(n) { var t = 0; for (var i = 0; i < n; i++) t += o1.mid(i); return t; }

function sloppyLeaf(i) { if (i === 3002) note('strict-between', sloppyLeaf); return i & 1; }
var st = { m: function strictMid(i) { 'use strict'; return sloppyLeaf(i) + 1; } };
function loop2(n) { var t = 0; for (var i = 0; i < n; i++) t += st.m(i); return t; }

var o3 = {
  inner: function inner(x) { x = x + 100; return leaf3(x); },
  outer: function outer(y) { y = y - 1; return o3.inner(y) + y; }
};
function leaf3(x) { if (x === 3102) { note('two-inner', o3.inner); note('two-outer', o3.outer); } return 1; }
function loop3(n) { var t = 0; for (var i = 0; i < n; i++) t += o3.outer(i + 3); return t; }

var cnt = 0;
var gobj = { get g() { if (cnt === 3002) note('getter', o4.mid); return 1; } };
var o4 = { mid: function mid4(a, b) { a = a * 2; cnt++; return gobj.g + a + b; } };
function loop4(n) { var t = 0; for (var i = 0; i < n; i++) { t += o4.mid(i, 7); if (i === 4000) note('returned', o4.mid); } return t; }

function Pt(x, y) { this.x = x; this.y = y; probe(x); }
function probe(x) { if (x === 3003) note('ctor', Pt); }
function loop5(n) { var t = 0; for (var i = 0; i < n; i++) t += new Pt(i, i + 1).y; return t; }

var o6 = { r: function r(k, d) { if (d === 0) { if (k === 3004) note('recursion', o6.r); return 1; } return o6.r(k, d - 1) + 1; } };
function loop6(n) { var t = 0; for (var i = 0; i < n; i++) t += o6.r(i, 2); return t; }

var want = [
  'one [[3001],"loop1"]', 'one-again [[3001],"loop1"]', 'strict-between [[3002],null]',
  'two-inner [[3102],"outer"]', 'two-outer [[3002],"loop3"]', 'getter [[6002,7],"loop4"]', 'returned [null,null]',
  'ctor [[3003,3004],"loop5"]', 'recursion [[3004,0],"r"]'
].join('|');
var bad = 0;
for (var round = 0; round < 3; round++) {
  got = []; cnt = 0;
  var sums = [loop1(5000), loop2(5000), loop3(5000), loop4(5000), loop5(5000), loop6(5000)].join(',');
  if (sums !== '2500,7500,12512500,25035000,12502500,15000' || got.join('|') !== want) { bad++; console.log(sums, got.join('|')); }
}
console.log(bad === 0 ? 'ok' : 'FAIL ' + bad);
