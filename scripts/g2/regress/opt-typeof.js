// optimized regions that inline switch (typeof x) and typeof comparisons (folded on a known kind): ints, doubles and
// booleans, then strings, objects, undefined and functions reaching the same code after it was optimized; sloppy
// callees called without this, inlined only when they never read it (a callee that does sees the global object)
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function kindOf(x) {
  switch (typeof x) {
    case 'undefined': return 0;
    case 'object': if (!x) return 1;
    case 'function': return 2;
    case 'boolean': return 3;
    case 'number': if ((x | 0) == x) return 4; return 5;
    case 'string': return 6;
    default: return 7;
  }
}
function same(a, b) { if (typeof a != typeof b) return false; return typeof a === 'number' ? a === b : a == b; }
function sloppyThis() { return this; }
function strictThis() { 'use strict'; return this; }

function sumKinds(n, f) { let s = 0; for (let i = 0; i < n; i++) s += kindOf(f(i)); return s; }
function countSame(n, f, g) { let c = 0; for (let i = 0; i < n; i++) if (same(f(i), g(i))) c++; return c; }
function thisLoop(n) { let g = 0, u = 0; for (let i = 0; i < n; i++) { if (sloppyThis() === globalThis) g++; if (strictThis() === undefined) u++; } return g + ':' + u; }

check('ints', sumKinds(5000, i => i), 4 * 5000);
check('doubles', sumKinds(5000, i => i + 0.5), 5 * 5000);
check('booleans', sumKinds(5000, i => (i & 1) === 0), 3 * 5000);
// the same optimized loop and callee meeting other kinds
check('strings', sumKinds(5000, i => 's' + i), 6 * 5000);
check('mixed', sumKinds(6, i => [undefined, null, {}, () => 1, 'x', 1.5][i]), 0 + 1 + 2 + 2 + 6 + 5);
check('symbol', kindOf(Symbol()), 7);
check('bigint', kindOf(10n), 7);

check('same ints', countSame(5000, i => i, i => i), 5000);
check('same int and string', countSame(5000, i => i, i => '' + i), 0);
check('same strings', countSame(5000, i => 'a' + (i & 7), i => 'a' + (i & 3)), 5000 / 2);
check('same int and double', countSame(5000, i => i, i => i + 0.5), 0);

check('this in sloppy and strict callees', thisLoop(5000), '5000:5000');

// numbers straight from arithmetic: the switch and the comparisons folded to their number cases
function sumNumbers(n) { let s = 0; for (let i = 0; i < n; i++) s += kindOf(i) + kindOf(i + 0.5) + (same(i, i) ? 1 : 0) + (same(i, i * 2) ? 1 : 0); return s; }
check('numbers folded', sumNumbers(5000), 5000 * (4 + 5 + 1) + 1);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
