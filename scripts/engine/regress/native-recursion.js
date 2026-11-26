// recursion through a native that calls back into code (toString through String, toJSON, a sort comparator,
// Reflect.apply, Reflect.construct, map, a getter) ends in a RangeError instead of overflowing the native stack
let bad = 0;
const cases = {
  getter: () => { const o = { get g() { return o.g; } }; return o.g; },
  toString: () => { const o = { toString() { return String(o); } }; return String(o); },
  toJSON: () => { const o = { toJSON() { return JSON.stringify(o); } }; return JSON.stringify(o); },
  sort: () => { function f() { return [2, 1].sort(() => f()); } return f(); },
  apply: () => { function f() { return Reflect.apply(f, null, []); } return f(); },
  ctor: () => { function C() { return Reflect.construct(C, []); } return new C(); },
  map: () => { function f() { return [1].map(f); } return f(); },
};
for (const [k, fn] of Object.entries(cases)) {
  let r;
  try { fn(); r = 'no error'; } catch (e) { r = e instanceof RangeError ? 'RangeError' : String(e); }
  if (r !== 'RangeError') { bad++; console.log('FAIL', k, r); }
}
if ([1, 2, 3].map(v => v + 1).join() !== '2,3,4') { bad++; console.log('FAIL after'); }
console.log(bad === 0 ? 'ok' : 'FAIL ' + bad);
