// WeakMap and WeakSet over ephemeron tables while collections run: a value stays reachable while its key does,
// through chains of keys held as values, across maps, with young and old keys and values, deletes and re-sets,
// non-registered symbol keys and a map that is itself only a value of another map
const churn = () => Array.from({ length: 300 }, (_, i) => ({ i, s: "x" + i }));
const out = [];
const emit = (...parts) => out.push(parts.join(" "));
const a = new WeakMap(), b = new WeakMap(), s = new WeakSet();
const roots = [];
for (let i = 0; i < 300; i++) {
  const k = { i };
  const v = { v: i, next: { deep: i } };
  a.set(k, v);
  if (i % 3 === 0) roots.push(k);
  if (i % 5 === 0) s.add(k);
  churn();
}
// a chain: each key's value is the next key, through two maps
const head = { h: 1 };
let key = head;
for (let i = 0; i < 2000; i++) { const n = { n: i }; (i % 2 ? a : b).set(key, n); key = n; }
// a map reachable only as a value of another map's entry
const inner = new WeakMap();
const outerKey = { o: 1 };
b.set(outerKey, inner);
const innerKey = { in: 1 };
inner.set(innerKey, "inner-value");
const sym = Symbol("k");
a.set(sym, { sym: true });
for (let r = 0; r < 20; r++) {
  churn();
  for (const k of roots) if (k.i % 2 === 0) a.set(k, { v: k.i, round: r, next: { deep: k.i } });
  if (r === 10) for (const k of roots) if (k.i % 9 === 0) a.delete(k);
}
let good = 0;
for (const k of roots) {
  const v = a.get(k);
  if (k.i % 9 === 0) { if (v === undefined) good++; }
  else if (v && v.v === k.i && v.next.deep === k.i) good++;
}
emit("roots", roots.length, "good", good);
emit("set", roots.filter((k) => s.has(k)).length);
let walked = 0, node = head;
while (node !== undefined) { walked++; node = (walked % 2 === 0 ? a : b).get(node); }
emit("chain", walked);
emit("inner", b.get(outerKey).get(innerKey));
emit("symbol", a.get(sym).sym, a.has(Symbol("k")));
const expected = [
  "roots 100 good 83",
  "set 20",
  "chain 2001",
  "inner inner-value",
  "symbol true false"
];
console.log(out.join("\n") === expected.join("\n") ? "ok" : "bad " + out.join(" | "));
