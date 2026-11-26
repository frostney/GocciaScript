// `#p in x` with x not an object, in a function called hot enough to compile: the compiled code throws the
// interpreter's TypeError, naming the private name and the value (a generated program found the JIT's own message)
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

class C {
  #p = 1;
  static has(x) { return #p in x; }
}
const want = (v) => "Cannot use 'in' operator to search for '#p' in " + v;
let last = '';
for (let i = 0; i < 2000; i++) {
  try { C.has(i % 2 ? 0 : 'a'); } catch (e) { last = e instanceof TypeError ? e.message : 'not a TypeError'; }
  if (i === 1999) check('0', last, want('0'));
  if (i === 1998) check('"a"', last, want('a'));
}
check('object', C.has(new C()), true);
check('other object', C.has({}), false);
console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
