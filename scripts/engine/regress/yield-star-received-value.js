// yield* hands the value its caller sent with return() or throw() to the delegate's method, which it reads by a
// getter first. A getter that allocates can run a collection that moves the young value; the step reads it again
// from its register after the getter (a copy taken before would be stale). Sync and async generators, return()
// with and without a return method, and throw()
const churn = () => Array.from({ length: 300 }, (_, i) => ({ i, s: "x" + i }));
const out = [];
const syncDelegate = (withReturn) => ({
  [Symbol.iterator]() { return this; },
  next() { return { value: 1, done: false }; },
  get return() { churn(); return withReturn ? (v) => ({ value: v, done: true }) : undefined; },
  get throw() { churn(); return (e) => ({ value: e, done: true }); },
});
for (const withReturn of [true, false]) {
  const g = (function* () { return yield* syncDelegate(withReturn); })();
  g.next();
  out.push(JSON.stringify(g.return({ tag: "sync-return" })));
}
{
  const g = (function* () { return yield* syncDelegate(true); })();
  g.next();
  out.push(JSON.stringify(g.throw({ tag: "sync-throw" })));
}
const asyncDelegate = (withReturn) => ({
  [Symbol.asyncIterator]() { return this; },
  next() { return { value: 1, done: false }; },
  get return() { churn(); return withReturn ? (v) => ({ value: v, done: true }) : undefined; },
});
(async () => {
  for (const withReturn of [true, false]) {
    const g = (async function* () { return yield* asyncDelegate(withReturn); })();
    await g.next();
    out.push(JSON.stringify(await g.return({ then(resolve) { resolve({ tag: "async-return" }); } })));
  }
  const expected = [
    '{"value":{"tag":"sync-return"},"done":true}',
    '{"value":{"tag":"sync-return"},"done":true}',
    '{"value":{"tag":"sync-throw"},"done":true}',
    '{"value":{"tag":"async-return"},"done":true}',
    '{"value":{"tag":"async-return"},"done":true}',
  ];
  console.log(out.join("\n") === expected.join("\n") ? "ok" : "bad " + out.join(" | "));
})();
