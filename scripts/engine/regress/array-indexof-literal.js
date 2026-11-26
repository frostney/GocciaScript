// Array.prototype.indexOf of a value compared by identity (objects, null, undefined, booleans, symbols) over dense
// arrays, holes, fromIndex forms (negative, past the end, fractional, NaN), array-likes and arrays changed between
// searches; and array literals of more than 64 elements with spreads and holes among them
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

const objs = Array.from({ length: 100 }, (_, i) => ({ i }));
const sym = Symbol('s');
const arr = objs.concat([null, undefined, true, false, sym]);
function search(a, x, from) { return from === undefined ? a.indexOf(x) : a.indexOf(x, from); }
let s = 0;
for (let r = 0; r < 300; r++) for (let i = 0; i < 100; i += 7) s += search(arr, objs[i]) + search(arr, objs[i], 50) + search(arr, objs[i], -20);
let want = 0;
for (let r = 0; r < 300; r++) for (let i = 0; i < 100; i += 7) want += i + (i >= 50 ? i : -1) + (i >= 85 ? i : -1);
check('objects', s, want);
check('null', arr.indexOf(null), 100);
check('undefined', arr.indexOf(undefined), 101);
check('true', arr.indexOf(true), 102);
check('false', arr.indexOf(false), 103);
check('symbol', arr.indexOf(sym), 104);
check('absent object', arr.indexOf({}), -1);
check('from past end', arr.indexOf(objs[0], 1000), -1);
check('from fraction', arr.indexOf(objs[3], 2.9), 3);
check('from NaN', arr.indexOf(objs[3], NaN), 3);
check('from -Infinity', arr.indexOf(objs[3], -Infinity), 3);
check('from at match', arr.indexOf(objs[3], 3), 3);
check('from after match', arr.indexOf(objs[3], 4), -1);
const holey = [objs[0], , objs[2], , undefined];
check('hole is not undefined', holey.indexOf(undefined), 4);
check('past a hole', holey.indexOf(objs[2]), 2);
check('array-like', Array.prototype.indexOf.call({ length: 3, 0: null, 2: objs[1] }, objs[1]), 2);
check('array-like hole', Array.prototype.indexOf.call({ length: 3, 0: null }, undefined), -1);
const grow = [objs[1]];
for (let i = 0; i < 2000; i++) { grow.push(objs[i % 100]); if (grow.indexOf(objs[i % 100]) !== (i % 100 === 1 ? 0 : (i < 100 ? i + 1 : 1 + (i % 100)))) { check('grow ' + i, false, true); break; } }
check('length shrunk', (grow.length = 5, grow.indexOf(objs[50])), -1);

// literals of more than 64 elements with spreads and holes among them
const big = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
  30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58,
  59, 60, 61, 62, 63, 64, ...[65, 66], , 68, ...'ab', , 72];
check('big length', big.length, 73);
check('big spread', big[65] + big[66], 131);
check('big hole', 67 in big, false);
check('big string spread', big[69] + big[70], 'ab');
check('big hole 2', 71 in big, false);
check('big last', big[72], 72);
check('small with hole', [1, , 3].length + ':' + (1 in [1, , 3]), '3:false');

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
