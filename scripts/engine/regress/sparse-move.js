// shift, unshift, splice and copyWithin move more than 4,096 indices of a sparse object by visiting only the
// indices present (SparseMoveRange). An own accessor on the way runs user code that adds, deletes and redefines
// indices, or gives Object.prototype an index, and the moves after it must see that, as Node.js does. The
// getters allocate, so collections run in the middle of a move
const churn = () => Array.from({ length: 200 }, (_, i) => ({ i }));
const out = [];
const emit = (...parts) => out.push(parts.join(" "));
const log = [];
const mk = () => {
  const o = { length: 10000, 5: "a", 9000: "b" };
  Object.defineProperty(o, 7000, { get() { log.push("get7000"); churn(); o[8000] = "added"; delete o[9000]; Object.defineProperty(o, 9500, { get() { log.push("get9500"); return "g"; }, configurable: true }); return "x"; }, configurable: true });
  Object.defineProperty(o, 3, { set(v) { log.push("set3:" + v); }, get() { return "s"; }, configurable: true });
  return o;
};
let o = mk();
Array.prototype.splice.call(o, 1, 3, "p");
emit(JSON.stringify(Object.keys(o)), JSON.stringify(log), o.length);
o = mk(); log.length = 0;
Array.prototype.unshift.call(o, "u1", "u2");
emit(JSON.stringify(Object.keys(o)), JSON.stringify(log), o.length);
o = mk(); log.length = 0;
Array.prototype.shift.call(o);
emit(JSON.stringify(Object.keys(o)), JSON.stringify(log), o.length);
o = mk(); log.length = 0;
Array.prototype.copyWithin.call(o, 2, 0);
emit(JSON.stringify(Object.keys(o).slice(0,20)), Object.keys(o).length, JSON.stringify(log), o.length);
const a = []; a.length = 9000; a[100] = 1; a[8999] = 2;
Object.defineProperty(a, 50, { get() { Object.defineProperty(Object.prototype, 4000, { value: "proto", configurable: true }); return "z"; }, configurable: true });
a.splice(0, 1);
emit(JSON.stringify(Object.keys(a)), a.length, a[3999]);
delete Object.prototype[4000];

// under 4,096 the moves run one index at a time (MoveRange): the receiver is reread after each getter collects
const small = { length: 3000, 10: "s", 2500: "t" };
Object.defineProperty(small, 1000, { get() { churn(); return "m"; }, configurable: true });
Array.prototype.splice.call(small, 0, 2, "n");
emit(JSON.stringify(Object.keys(small)), small.length, small[999]);

const expected = [
  "[\"1\",\"6998\",\"7998\",\"9498\",\"length\"] [\"set3:a\",\"get7000\",\"get9500\"] 9998",
  "[\"0\",\"1\",\"5\",\"7\",\"7002\",\"8000\",\"9002\",\"length\"] [\"get7000\"] 10002",
  "[\"2\",\"4\",\"6999\",\"7999\",\"9499\",\"length\"] [\"get7000\",\"get9500\"] 9999",
  "[\"5\",\"7\",\"7002\",\"8000\",\"9002\",\"length\"] 6 [\"get7000\"] 10000",
  "[\"49\",\"99\",\"3999\",\"8998\"] 8999 proto",
  "[\"0\",\"9\",\"999\",\"2499\",\"length\"] 2999 m"
];
console.log(out.join("\n") === expected.join("\n") ? "ok" : "bad " + out.join(" | "));
