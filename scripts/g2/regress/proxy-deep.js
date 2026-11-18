// native recursion through Proxies ends in a RangeError and leaves the engine sound: a prototype cycle closed by a
// Proxy (reads, has, sets, instanceof and isPrototypeOf, which walk it in a loop), 100,000 nested Proxies, and
// callable Proxies nested as targets; then allocation and an await keep working (the temp root stack and the native stack
// both outlast the recursion)
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }
function throwsRange(f) { try { f(); return false; } catch (e) { return e instanceof RangeError; } }

const makeCycle = () => { const o = {}; const p = new Proxy(Object.create(o), {}); Object.setPrototypeOf(o, p); return { o, p }; };
for (const [name, f] of [['read', (x) => x.missing], ['has', (x) => 'k' in x], ['set', (x) => { x.k = 1; }],
  ['toString', (x) => String(x)], ['instanceof', (x) => x instanceof (class U {})],
  ['isPrototypeOf', (x) => (class U {}).prototype.isPrototypeOf(x)]]) {
  check('cycle ' + name, throwsRange(() => f(makeCycle().o)), true);
  check('cycle via proxy ' + name, throwsRange(() => f(makeCycle().p)), true);
}
const nest = (depth) => { let p = { x: 'found' }; for (let i = 0; i < depth; i++) p = new Proxy(p, {}); return p; };
check('1000 nested', nest(1000).x, 'found');
check('100000 nested', throwsRange(() => nest(100000).x), true);
// callable Proxies nested as targets: [[Call]] forwards natively (a bound chain would too, but its names grow with
// the square of its depth)
let f = () => 7;
for (let i = 0; i < 100000; i++) f = new Proxy(f, {});
check('call chain', throwsRange(() => f()), true);
check('bound twice', ((() => 7).bind(null).bind(null))(), 7);

let after = 0;
async function go() {
  for (let k = 0; k < 3; k++) {
    check('nested in async ' + k, throwsRange(() => nest(100000).x), true);
    await null;
    const junk = Array.from({ length: 2000 }, (_, i) => ({ i }));
    after += junk.length;
  }
}
go().then(() => {
  check('after awaits', after, 6000);
  console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
});
