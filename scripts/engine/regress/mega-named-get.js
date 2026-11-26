// named gets at sites that have seen more shapes than their inline cache holds (the megamorphic cache, entries
// copied into the site's cache in turn): own and prototype data properties, absent names, a dictionary prototype
// changed in place, accessors among them, objects changed after the caches held them, and phases where the set of
// shapes a site sees shifts
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function nameOf(o) { return o.name; }
function sumNames(objs, n) {
  let s = 0;
  for (let r = 0; r < n; r++) for (let i = 0; i < objs.length; i++) { const v = nameOf(objs[i]); s += v === undefined ? 1000 : v; }
  return s;
}
function slow(objs, n) {
  let s = 0;
  for (let r = 0; r < n; r++) for (let i = 0; i < objs.length; i++) { const v = Reflect.get(objs[i], 'name'); s += v === undefined ? 1000 : v; }
  return s;
}

function make(k, withName) { const o = {}; for (let j = 0; j <= k; j++) o['p' + j] = j; if (withName) o.name = k; return o; }
const many = [];
for (let k = 0; k < 16; k++) many.push(make(k, k % 3 !== 0));
const dict = {}; for (let i = 0; i < 50; i++) dict['d' + i] = i; for (let i = 0; i < 48; i++) delete dict['d' + i];
dict.name = 77;
const viaDict = Object.create(dict);
class Base { } Base.prototype.name = 5; class Mid extends Base { } class Leaf extends Mid { }
let getterCalls = 0;
const acc = { get name() { getterCalls++; return 3; } };
const all = many.concat([viaDict, new Leaf(), new Mid(), Object.create(null), acc]);

for (let round = 0; round < 3; round++) {
  check('mixed ' + round, sumNames(all, 500), slow(all, 500));
  dict.name = 80 + round;                 // a dictionary prototype changed in place
  Base.prototype.name = round;            // a class prototype changed
  many[4].name = 100 + round;             // an object changed
  delete many[5].name;                    // a name removed
}
// phases: the set of shapes shifts, each phase's shapes seen many times in a row
for (let ph = 0; ph < 4; ph++) {
  const set = many.slice(ph * 4, ph * 4 + 4).concat([new Leaf()]);
  check('phase ' + ph, sumNames(set, 2000), slow(set, 2000));
}
check('getter calls', getterCalls > 0, true);
const g0 = getterCalls; nameOf(acc); check('getter again', getterCalls, g0 + 1);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
