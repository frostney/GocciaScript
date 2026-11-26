// WeakRef and FinalizationRegistry over Goccia.Weak's boxes while collections move and free their targets: a target
// still reachable is never cleared, and a cleanup callback receives only held values of targets that were dropped,
// each once. Young boxes, young targets and old ones mix; the churn makes the nursery collect throughout
const churn = () => Array.from({ length: 200 }, (_, i) => ({ i, s: "x" + i }));
const kept = [];
const refs = [];
const seen = new Set();
let bad = 0;
const registry = new FinalizationRegistry((held) => {
  if (typeof held !== "string" || !held.startsWith("dropped") || seen.has(held)) bad++;
  seen.add(held);
});
for (let i = 0; i < 400; i++) {
  const target = { n: i, payload: churn().length };
  if (i % 2 === 0) {
    kept.push(target);
    registry.register(target, "kept" + i, target);
  } else {
    registry.register(target, "dropped" + i);
  }
  refs.push([i, new WeakRef(target)]);
  churn();
}
const sym = Symbol("weak");
const symRef = new WeakRef(sym);
const check = () => {
  for (const [i, ref] of refs) {
    const t = ref.deref();
    if (i % 2 === 0 && (t === undefined || t.n !== i)) bad++;
    if (t !== undefined && t.n !== i) bad++;
  }
  if (symRef.deref() !== sym) bad++;
};
check();
// each round is a job of its own (a promise reaction), so the kept list of the round before is cleared
let p = Promise.resolve();
for (let round = 0; round < 20; round++) p = p.then(() => { churn(); check(); });
p.then(() => console.log(bad === 0 && registry.unregister(kept[0]) ? "ok" : "bad " + bad));
