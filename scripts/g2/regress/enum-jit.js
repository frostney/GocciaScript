// enum declarations (TC39 proposal-enum, GocciaScript's semantics) in a function called hot enough to compile:
// opFinalizeEnum leaves compiled code for the interpreter; members reference earlier members and the enum itself,
// and the enum is frozen-shaped (null prototype, non-extensible, read-only members, entries iteration)
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function mk(i) { enum E { A = i, B = A + 1, C = "c" + E.A } return E; }
let s = 0;
for (let i = 0; i < 20000; i++) { const E = mk(i); s += E.B + E.C.length; }
let want = 0;
for (let i = 0; i < 20000; i++) want += (i + 1) + ('c' + i).length;
check('hot enums', s, want);
const E = mk(7);
check('null prototype', Object.getPrototypeOf(E), null);
check('non-extensible', Object.isExtensible(E), false);
check('read-only', Object.getOwnPropertyDescriptor(E, 'A').writable, false);
check('entries', JSON.stringify([...E]), '[["A",7],["B",8],["C","c7"]]');
check('tag', Object.prototype.toString.call(E), '[object E]');
let threw = false;
try { (() => { enum Bad { X = {} } return Bad; })(); } catch (e) { threw = e instanceof TypeError; }
check('object member refused', threw, true);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
