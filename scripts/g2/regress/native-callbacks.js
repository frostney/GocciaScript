// compiled functions called from natives (getters, forEach / map / reduce callbacks, Reflect.apply, call /
// apply) get their this as a call would give it, their missing parameters undefined, and their throws and
// stack overflows through the native
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }
const globalObj = (function () { return this; })();
class P { constructor(x) { this._x = x; } get x() { return this._x * 2; } }
const ps = []; for (let i = 0; i < 20; i++) ps.push(new P(i));
let s = 0;
for (let r = 0; r < 2000; r++) for (const p of ps) s += p.x;
check('getters', s, 2000 * 380);
function sloppyThis() { return this; }
function strictThis() { 'use strict'; return this; }
let sloppyOk = 0, strictOk = 0, boxOk = 0, strictPrim = 0;
for (let r = 0; r < 1500; r++) {
  [1].forEach(function () { if (this === globalObj) sloppyOk++; });
  [1].forEach(function () { 'use strict'; if (this === undefined) strictOk++; });
  [1].forEach(function () { if (typeof this === 'object' && this instanceof Number) boxOk++; }, 5);
  [1].forEach(function () { 'use strict'; if (this === 5) strictPrim++; }, 5);
}
check('sloppy undefined this is the global object', sloppyOk, 1500);
check('strict undefined this', strictOk, 1500);
check('sloppy primitive this boxed', boxOk, 1500);
check('strict primitive this kept', strictPrim, 1500);
function three(a, b, c) { return [a, b, c].map(v => v === undefined ? 'u' : v).join(''); }
let m = '';
for (let r = 0; r < 1500; r++) m = Reflect.apply(three, null, [1]) + three.call(null, 1, 2, 3, 4) + three.apply(null, []);
check('missing and extra arguments', m, '1uu123uuu');
let caught = 0;
for (let r = 0; r < 1500; r++) { try { [1, 2].map(v => { if (v === 2) throw new Error('x' + v); return v; }); } catch (e) { if (e.message === 'x2') caught++; } }
check('throw through map', caught, 1500);
let sum = 0;
for (let r = 0; r < 1500; r++) sum += [1, 2, 3, 4].reduce((a, b) => a + b, 0);
check('reduce', sum, 15000);
function deep(n) { let c = 0; [1].forEach(() => { c = deep(n + 1) + 1; }); return c; }
let overflow = false;
try { deep(0); } catch (e) { overflow = e instanceof RangeError; }
check('stack overflow through forEach is a RangeError', overflow, true);
check('still running after the overflow', [1, 2, 3].map(v => v * 2).join(), '2,4,6');
console.log(bad === 0 ? 'ok' : 'FAIL ' + bad);
