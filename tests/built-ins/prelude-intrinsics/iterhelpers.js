/*---
description: The iterhelpers prelude's Iterator and its helpers look up nothing a script can replace, beyond the iterator protocol ECMA-262 reads
features: [Iterator, iterator-helpers, iterator-sequencing, joint-iteration]
---*/

import { ARRAY_ITERATION, expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

// GetIteratorDirect reads the underlying iterator's next once (27.1.3.2.x), GetIteratorFlattenable an iterable's
// Symbol.iterator method; both are the array iterator's here, and its return is looked up when a helper closes it
const PROTOCOL = [...ARRAY_ITERATION, "%ArrayIteratorPrototype%.return"];
const iter = () => [1, 2, 3, 4][Symbol.iterator]();
const call = (path, thisArg, ...args) => expectClean(wire.invoke(path, thisArg, ...args), PROTOCOL);
const drain = (it) => expectClean(wire.watch(() => {
  const out = [];
  const next = wire.original("%IteratorHelperPrototype%.next");
  const step = () => {
    const r = wire.call(next, it);
    if (r.done) return out;
    out[out.length] = r.value;
    return step();
  };
  return step();
}), PROTOCOL);

test("the Iterator.prototype helpers", () => {
  const P = "Iterator.prototype";
  expect(drain(call(`${P}.map`, iter(), (x) => x * 2))).toEqual([2, 4, 6, 8]);
  expect(drain(call(`${P}.filter`, iter(), (x) => x % 2))).toEqual([1, 3]);
  expect(drain(call(`${P}.take`, iter(), 2))).toEqual([1, 2]);
  expect(drain(call(`${P}.drop`, iter(), 3))).toEqual([4]);
  expect(drain(call(`${P}.flatMap`, iter(), (x) => [x, x]))).toEqual([1, 1, 2, 2, 3, 3, 4, 4]);
  expect(call(`${P}.reduce`, iter(), (a, b) => a + b)).toBe(10);
  expect(call(`${P}.toArray`, iter())).toEqual([1, 2, 3, 4]);
  expect(call(`${P}.forEach`, iter(), () => {})).toBe(undefined);
  expect(call(`${P}.some`, iter(), (x) => x > 3)).toBe(true);
  expect(call(`${P}.every`, iter(), (x) => x > 3)).toBe(false);
  expect(call(`${P}.find`, iter(), (x) => x > 2)).toBe(3);
  expect(drain(call(`${P}.chunks`, iter(), 3))).toEqual([[1, 2, 3], [4]]);
  expect(drain(call(`${P}.windows`, iter(), 3))).toEqual([[1, 2, 3], [2, 3, 4]]);
  expect(call(`${P}.includes`, iter(), 3)).toBe(true);
  expect(call(`${P}.join`, iter(), "-")).toBe("1-2-3-4");
  const it = iter();
  expect(call(`${P}[Symbol.iterator]`, it)).toBe(it);
  expect(call(`${P}[Symbol.dispose]`, iter())).toBe(undefined);
  expect(call(`${P}.constructor`, it)).toBe(Iterator);
  expect(call(`${P}[Symbol.toStringTag]`, it)).toBe("Iterator");
  const helper = call(`${P}.map`, iter(), (x) => x);
  wire.covers("%IteratorHelperPrototype%", helper, [
    ["next", [], { value: 1, done: false }, PROTOCOL], ["return", [], { value: undefined, done: true }, PROTOCOL],
  ]);
});

test("Iterator, Iterator.from, concat, zip and zipKeyed", () => {
  class Counter extends Iterator { next() { return { done: true, value: undefined }; } }
  const abstract = wire.construct("globalThis.Iterator");
  expect(abstract.threw).toBe(true);
  expect(abstract.hits).toEqual([]);
  const wrapped = call("Iterator.from", Iterator, { next() { return { done: false, value: 1 }; } });
  wire.covers("%WrapForValidIteratorPrototype%", wrapped, [
    ["next", [], { done: false, value: 1 }], ["return", [], { done: true, value: undefined }],
  ]);
  expect(call("Iterator.from", Iterator, [1, 2]).toArray()).toEqual([1, 2]);
  expect(drain(call("Iterator.concat", Iterator, [1], [2, 3]))).toEqual([1, 2, 3]);
  expect(drain(call("Iterator.zip", Iterator, [[1, 2], ["a", "b"]], { mode: "shortest" }))).toEqual([[1, "a"], [2, "b"]]);
  expect(drain(call("Iterator.zipKeyed", Iterator, { x: [1], y: [2] }))).toEqual([{ x: 1, y: 2 }]);
  expect(new Counter().next().done).toBe(true);
});

test("every Iterator built-in has a case", () => {
  const owners = ["Iterator", "Iterator.prototype", "%IteratorHelperPrototype%", "%WrapForValidIteratorPrototype%"];
  expect(owners.flatMap((o) => wire.uncovered(o))).toEqual([]);
});
