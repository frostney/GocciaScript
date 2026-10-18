// Heap snapshot workload: references to built-ins the run materialized
// lazily (JSON, Temporal), a realm intrinsic created on first use (the
// array iterator prototype), a registered and a local symbol. A restore
// resolves each by its path and materializes what the path crosses.

const parse = JSON.parse;
const tzid = Temporal.Now.timeZoneId;
const max = Math.max;
const keys = Object.keys;
const big = Temporal.Instant;
const iterProto = Object.getPrototypeOf([][Symbol.iterator]());
const sym = Symbol.for("app.key");
const local = Symbol("local");
const box = { [sym]: 1, [local]: 2, tag: Symbol.iterator };
const answer = await hostCall("x", 1);
print(parse('{"a":1}').a, typeof tzid(), max(1, answer), keys(box).length, typeof big.fromEpochMilliseconds, iterProto === Object.getPrototypeOf([][Symbol.iterator]()), box[sym], box[local], box.tag === Symbol.iterator, Symbol.keyFor(sym));
