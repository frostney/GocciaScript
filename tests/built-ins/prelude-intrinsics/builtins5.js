/*---
description: The builtins5 prelude's built-ins and language helpers look up nothing a script can replace, beyond what ECMA-262 reads
features: [Array.fromAsync, Map.prototype.getOrInsert, Math.sumPrecise, Uint8Array, WeakRef, FinalizationRegistry, enum-declaration, pattern-matching, decorators]
---*/

import { ARRAY_ITERATION, expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());
const call = (path, thisArg, ...args) => expectClean(wire.invoke(path, thisArg, ...args));

test("Object, Map and Array statics", () => {
  const grouped = expectClean(wire.invoke("Object.groupBy", Object, [1, 2, 3], (x) => (x % 2 ? "odd" : "even")), ARRAY_ITERATION);
  expect(grouped.odd).toEqual([1, 3]);
  const r = wire.invoke("Map.groupBy", Map, [1, 2, 3], (x) => x % 2);
  expect(expectClean(r, ARRAY_ITERATION).get(1)).toEqual([1, 3]);
  expect(expectClean(wire.invoke("Object.fromEntries", Object, [["a", 1]]), ARRAY_ITERATION)).toEqual({ a: 1 });
  expect(call("Object.hasOwn", Object, { a: 1 }, "a")).toBe(true);
  expect(call("String.fromCodePoint", String, 0x1f600, 65)).toBe("\u{1F600}A");
  expect(call("String.raw", String, { raw: ["a", "b"] }, 1)).toBe("a1b");
  expect(call("BigInt.asIntN", BigInt, 8, 255n)).toBe(-1n);
  expect(call("BigInt.asUintN", BigInt, 8, -1n)).toBe(255n);
  expect(expectClean(wire.invoke("Math.sumPrecise", Math, [1e20, 0.1, -1e20]), ARRAY_ITERATION)).toBe(0.1);
});

test("Array.fromAsync", async () => {
  const r = await wire.invokeAsync("Array.fromAsync", Array, [1, Promise.resolve(2)], (x) => x * 10);
  // the iterable's own iterator (ECMA-262 Array.fromAsync step 3.e), and each value awaited (Await reads a promise's
  // constructor, 27.2.4.7 PromiseResolve)
  expect(expectClean(r, [...ARRAY_ITERATION, "Promise.prototype.constructor"])).toEqual([10, 20]);
});

test("Promise.allKeyed and Promise.allSettledKeyed", async () => {
  const a = await wire.invokeAsync("Promise.allKeyed", Promise, { x: 1, y: Promise.resolve(2) });
  // each value goes through Promise.resolve and its then (27.2.4.1.1 steps), as Promise.all's do
  const promiseSteps = ["Promise.resolve", "Promise.prototype.constructor", "Promise.prototype.then", "Promise[Symbol.species] getter"];
  expect(expectClean(a, promiseSteps)).toEqual({ x: 1, y: 2 });
  const s = await wire.invokeAsync("Promise.allSettledKeyed", Promise, { x: Promise.reject(1) });
  expect(expectClean(s, promiseSteps)).toEqual({ x: { status: "rejected", reason: 1 } });
});

test("Map.prototype.getOrInsert and getOrInsertComputed", () => {
  const m = new Map();
  wire.covers("Map.prototype", m, [["getOrInsert", ["a", 1], 1], ["getOrInsertComputed", ["b", () => 2], 2]]);
});

test("WeakMap, WeakSet, WeakRef and FinalizationRegistry", () => {
  const key = {};
  // the constructors iterate their argument and call the adder they look up (24.1.1.2 AddEntriesFromIterable)
  const wm = expectClean(wire.construct("globalThis.WeakMap", [[key, 1]]), [...ARRAY_ITERATION, "WeakMap.prototype.set"]);
  wire.covers("WeakMap.prototype", wm, [
    ["get", [key], 1], ["set", [{}, 2]], ["has", [key], true], ["delete", [{}], false],
    ["getOrInsert", [key, 5], 1], ["getOrInsertComputed", [{}, () => 3], 3],
  ]);
  const ws = expectClean(wire.construct("globalThis.WeakSet", [key]), [...ARRAY_ITERATION, "WeakSet.prototype.add"]);
  wire.covers("WeakSet.prototype", ws, [["add", [{}]], ["has", [key], true], ["delete", [key], true]]);
  const ref = expectClean(wire.construct("globalThis.WeakRef", key));
  wire.covers("WeakRef.prototype", ref, [["deref", [], key]]);
  const fr = expectClean(wire.construct("globalThis.FinalizationRegistry", () => {}));
  const token = {};
  wire.covers("FinalizationRegistry.prototype", fr, [["register", [{}, "held", token]], ["unregister", [token], true]]);
});

test("Uint8Array base64 and hex", () => {
  const bytes = call("Uint8Array.fromBase64", Uint8Array, "AQID", { alphabet: "base64url", lastChunkHandling: "loose" });
  expect(Array.from(bytes)).toEqual([1, 2, 3]);
  expect(Array.from(call("Uint8Array.fromHex", Uint8Array, "0a0b"))).toEqual([10, 11]);
  wire.covers("Uint8Array.prototype", new Uint8Array([1, 2, 255]), [
    ["toBase64", [{ omitPadding: true }], "AQL/"], ["toHex", [], "0102ff"],
    ["setFromBase64", ["AAAA"], { read: 4, written: 3 }], ["setFromHex", ["ff"], { read: 2, written: 1 }],
  ]);
});

test("the enum, pattern matching and decorator helpers", () => {
  const { customMatcher, metadata } = { customMatcher: Symbol.customMatcher, metadata: Symbol.metadata };
  const r = wire.watch(() => {
    enum Color { Red = "r", Green = "g" }
    class Point { constructor(x) { this.x = x; } }
    const matcher = { [customMatcher](v) { return v === 7 ? [v] : false; } };
    const tag = (value, context) => {
      context.addInitializer(() => {});
      context.metadata.tagged = true;
      return value;
    };
    @tag class Decorated { @tag method() {} @tag static field = 1; @tag accessor auto = 2; }
    let tail = null;
    if ([1, 2, 3] is [1, ...const rest]) tail = rest;
    let matched = null;
    if (7 is matcher(const seven)) matched = seven;
    return [
      Color.Green,
      tail,
      new Point(1) is Point,
      matched,
      { a: 1 } is { a: 1 },
      Decorated[metadata].tagged,
    ];
  });
  // the array pattern iterates the matched array (proposal-pattern-matching, ListPattern)
  expect(expectClean(r, ARRAY_ITERATION)).toEqual(["g", [2, 3], true, 7, true, true]);
});

test("every builtins5 built-in has a case", () => {
  const paths = ["WeakMap.prototype", "WeakSet.prototype", "WeakRef.prototype", "FinalizationRegistry.prototype"];
  expect(paths.flatMap((p) => wire.uncovered(p))).toEqual([]);
});
