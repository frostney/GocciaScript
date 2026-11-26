// Proxy [[DefineOwnProperty]] keeps the descriptor it was handed reachable across the trap's lookup, which runs
// guest code (a handler accessor) that allocates and collects: the descriptor's value is a fresh object nothing else
// holds. Collections happen there under gcstress.sh's small nurseries; the plain run checks the values arrive.
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

const churn = () => { let t = 0; for (let i = 0; i < 400; i++) { const s = { a: i, b: [i, i + 1], c: 'x' + i }; t += s.b[1]; } return t; };
const handler = (record) => ({
  get defineProperty() {
    churn();
    return (target, key, desc) => { record.seen = desc.value; return Reflect.defineProperty(target, key, desc); };
  }
});
const plain = { get defineProperty() { churn(); return undefined; } };
let ok = 0;
for (let i = 0; i < 300; i++) {
  const record = {}, target = {};
  const proxy = new Proxy(target, handler(record));
  Object.defineProperty(proxy, 'p' + (i & 7), { get value() { return { tag: 'v' + i }; }, enumerable: true, configurable: true });
  if (record.seen.tag === 'v' + i && target['p' + (i & 7)].tag === 'v' + i) ok++;
  // a trap the lookup finds undefined: the target's own [[DefineOwnProperty]] gets the same descriptor
  const t2 = {};
  Reflect.defineProperty(new Proxy(t2, plain), Symbol.for('k'), { value: { n: i }, enumerable: true, configurable: true });
  if (t2[Symbol.for('k')].n === i) ok++;
}
check('descriptors survive', ok, 600);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
