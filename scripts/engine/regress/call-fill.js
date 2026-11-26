// the registers of a compiled callee past its arguments start undefined: calls and constructions with fewer,
// as many and more arguments than parameters, small and large frames (registers past 16 and 32), uninitialized
// vars read before any store, after calls that left other values in the same stack slots, and under collections
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function small(a, b) { var x; var y; return [a, b, x, y]; }
function big(a, b, c) {
  var v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19;
  var w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17, w18, w19;
  const all = [a, b, c, v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19,
    w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17, w18, w19];
  return all;
}
function dirty(n) {
  // leaves objects and numbers in the stack slots the next callee's registers take
  const o = { n }, p = [n, n], q = 'q' + n, r = n * 1.5, s = { o, p }, t = [s, s], u = { t };
  return u.t.length + n;
}
function undefCount(arr, from) { let k = 0; for (let i = from; i < arr.length; i++) if (arr[i] === undefined) k++; return k; }

for (let i = 0; i < 5000; i++) {
  dirty(i);
  const r0 = small();
  const r1 = small(1);
  const r2 = small(1, 2);
  const r3 = small(1, 2, 3, 4, 5);
  if (r0.join() !== ',,,' || r1[0] !== 1 || r1[1] !== undefined || r2[1] !== 2 || r3[1] !== 2 ||
      undefCount(r0, 0) !== 4 || undefCount(r1, 1) !== 3 || undefCount(r2, 2) !== 2 || undefCount(r3, 2) !== 2) {
    check('small ' + i, JSON.stringify([r0, r1, r2, r3]), 'all undefined past the arguments');
    break;
  }
  dirty(i);
  const b0 = big();
  const b1 = big(1);
  const b3 = big(1, 2, 3);
  const b6 = big(1, 2, 3, 4, 5, 6);
  if (undefCount(b0, 0) !== 43 || undefCount(b1, 1) !== 42 || undefCount(b3, 3) !== 40 || undefCount(b6, 3) !== 40 ||
      b6[2] !== 3) {
    check('big ' + i, [undefCount(b0, 0), undefCount(b1, 1), undefCount(b3, 3), undefCount(b6, 3)].join(), '43,42,40,40');
    break;
  }
}

// constructions
function Pt(x, y) { var z; this.x = x; this.y = y; this.z = z; }
class C { constructor(a, b) { let l; this.s = [a, b, l]; } }
for (let i = 0; i < 5000; i++) {
  dirty(i);
  const p = new Pt(i);
  const c = new C(i);
  const c2 = new C(i, i, i, i);
  if (p.x !== i || p.y !== undefined || p.z !== undefined || c.s[1] !== undefined || c.s[2] !== undefined ||
      c2.s[1] !== i || c2.s[2] !== undefined) {
    check('construct ' + i, JSON.stringify([p, c.s, c2.s]), 'undefined past the arguments');
    break;
  }
}

// methods and closures, with collections in between
const objs = [];
function Mk(k) { this.k = k; }
Mk.prototype.m = function (a, b, c) { var t; return (a === undefined ? 0 : 1) + (b === undefined ? 0 : 2) + (c === undefined ? 0 : 4) + (t === undefined ? 0 : 8); };
for (let i = 0; i < 100000; i++) {
  const o = new Mk(i);
  if (i % 100 === 0) objs.push(o);
  const want = [0, 1, 3, 7][i % 4];
  const got = i % 4 === 0 ? o.m() : i % 4 === 1 ? o.m(1) : i % 4 === 2 ? o.m(1, 2) : o.m(1, 2, 3, 4);
  if (got !== want) { check('method ' + i, got, want); break; }
}
check('kept', objs.length, 1000);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
