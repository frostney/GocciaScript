// element receivers checked once at an optimized region's entry: loops over arrays and typed arrays, an array
// that grows inside the loop (its store replaced), receivers reassigned inside the loop (checked each time),
// and the same optimized code then given primitives, strings, plain objects and other kinds of receiver
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }
function errName(f) { try { f(); return 'none'; } catch (e) { return e.constructor.name; } }

function sum(a, n) { let s = 0; for (let i = 0; i < n; i++) s += a[i]; return s; }
function fillGrow(a, n) { for (let i = 0; i < n; i++) { a[i] = i; if (i === a.length - 1) a.push(0); } return a.length; }
function copy(src, dst, n) { for (let i = 0; i < n; i++) dst[i] = src[i] * 2; return dst; }
function swapRecv(a, b, n) { let s = 0, x = a; for (let i = 0; i < n; i++) { s += x[i & 3]; x = (i & 1) ? a : b; } return s; }

const arr = Array.from({ length: 1000 }, (_, i) => i);
const f64 = new Float64Array(1000).map((_, i) => i / 2);
let r = 0;
for (let k = 0; k < 200; k++) r += sum(arr, 1000);
check('array sum', r, 200 * 499500);
r = 0;
for (let k = 0; k < 200; k++) r += sum(f64, 1000);
check('typed sum', r, 200 * 249750);

// growing inside the loop: the store moves, the receiver does not
for (let k = 0; k < 100; k++) { const a = [0, 0]; if (fillGrow(a, 500) !== 501) { check('grow ' + k, a.length, 501); break; } }

// two receivers, one read and one written
const dst = new Array(1000).fill(0);
for (let k = 0; k < 100; k++) copy(arr, dst, 1000);
check('copy', dst[999], 1998);

// a receiver reassigned in the loop
for (let k = 0; k < 200; k++) r = swapRecv([1, 2, 3, 4], [10, 20, 30, 40], 1000);
check('reassigned receiver', r, swapRecv([1, 2, 3, 4], [10, 20, 30, 40], 1000));

// the optimized code given other receivers after warming up
check('string receiver', sum('12345', 5), '012345');
check('plain object', sum({ 0: 1, 1: 2, 2: 3 }, 3), 6);
check('arguments-like', sum({ length: 2, 0: 5, 1: 6 }, 2), 11);
check('undefined receiver', errName(() => sum(undefined, 3)), 'TypeError');
check('number receiver', sum(5, 2), NaN);
check('array again', sum(arr, 10), 45);
check('typed again', sum(f64, 4), 3);
check('copy into typed', copy(arr, new Int32Array(5), 5).join(), '0,2,4,6,8');

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
