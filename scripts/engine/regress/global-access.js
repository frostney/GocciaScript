// reads and writes of global variables from compiled code: var properties of the global object (in its
// overflow slots), let and const bindings (open while this script runs, then read and written by functions),
// their TDZ, assignment to a const, a property deleted, made read-only or turned into an accessor, and stores
// of new objects into old cells
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }
function throwsName(f) { try { f(); return 'none'; } catch (e) { return e.constructor.name; } }

// a top-level loop on var globals
var vs = 0;
for (var vi = 0; vi < 5000; vi++) vs = (vs + vi) | 0;
check('top-level var loop', vs, 12497500);

// a top-level loop on let globals
let ls = 0;
for (let li = 0; li < 5000; li++) ls += li;
check('top-level let loop', ls, 12497500);

// functions reading and writing let bindings, and a var
let counter = 0;
var vcounter = 0;
function bump() { counter++; vcounter += 2; return counter; }
for (let i = 0; i < 3000; i++) bump();
check('let written by a function', counter, 3000);
check('var written by a function', vcounter, 6000);
counter = 2.5;
check('let rewritten', bump(), 3.5);

// TDZ: a hot function that reads and writes a binding declared further down
function readLate() { return late; }
function writeLate(v) { late = v; }
for (let i = 0; i < 3000; i++) { check('tdz read', throwsName(readLate), 'ReferenceError'); if (bad) break; }
for (let i = 0; i < 3000; i++) { check('tdz write', throwsName(() => writeLate(1)), 'ReferenceError'); if (bad) break; }
let late = 5;
check('read after init', readLate(), 5);
writeLate(6);
check('write after init', late, 6);
for (let i = 0; i < 3000; i++) writeLate(i);
check('hot write after init', readLate(), 2999);

// a const: hot reads, writes throw every time
const K = 42;
function readK() { return K; }
function writeK() { K = 1; }
let ks = 0;
for (let i = 0; i < 3000; i++) ks += readK();
check('const reads', ks, 126000);
for (let i = 0; i < 3000; i++) { check('const write', throwsName(writeK), 'TypeError'); if (bad) break; }
check('const kept', K, 42);

// a property of the global object deleted, then read (ReferenceError) and written (sloppy: created again)
globalThis.gp = 1;
function readGp() { return gp; }
function writeGp(v) { gp = v; }
for (let i = 0; i < 3000; i++) writeGp(readGp() + 1);
check('global property', gp, 3001);
delete globalThis.gp;
check('deleted read', throwsName(readGp), 'ReferenceError');
writeGp(7);
check('sloppy write creates it', readGp(), 7);

// made read-only: a sloppy write is ignored, a strict one throws
globalThis.ro = 1;
function writeRo(v) { ro = v; }
function writeRoStrict(v) { 'use strict'; ro = v; }
for (let i = 0; i < 3000; i++) { writeRo(i); writeRoStrict(i); }
check('writable', ro, 2999);
Object.defineProperty(globalThis, 'ro', { writable: false });
writeRo(5);
check('read-only sloppy', ro, 2999);
check('read-only strict', throwsName(() => writeRoStrict(5)), 'TypeError');

// turned into an accessor
globalThis.acc = 1;
function readAcc() { return acc; }
function writeAcc(v) { acc = v; }
for (let i = 0; i < 3000; i++) writeAcc(readAcc() + 1);
let stored;
Object.defineProperty(globalThis, 'acc', { get() { return 'got'; }, set(v) { stored = v; }, configurable: true });
check('accessor read', readAcc(), 'got');
writeAcc('w');
check('accessor write', stored, 'w');

// undeclared in strict code
function writeUndeclared() { 'use strict'; undeclaredXyz = 1; }
check('strict undeclared', throwsName(writeUndeclared), 'ReferenceError');

// new objects stored into old cells survive collections
var keepVar = null;
let keepLet = null;
function churn(n) {
  for (let i = 0; i < n; i++) {
    keepVar = { i, next: keepVar && keepVar.i };
    keepLet = [i, keepLet ? keepLet[0] : -1];
    const junk = new Array(16).fill(i);
    if (junk.length !== 16) bad++;
  }
}
churn(200000);
check('var cell after GC', keepVar.i + ':' + keepVar.next, '199999:199998');
check('let cell after GC', keepLet[0] + ':' + keepLet[1], '199999:199998');

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
