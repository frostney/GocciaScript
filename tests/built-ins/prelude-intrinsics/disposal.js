/*---
description: The disposal prelude's DisposableStack, AsyncDisposableStack, SuppressedError and using helpers look up nothing a script can replace, beyond what ECMA-262 reads
features: [explicit-resource-management]
---*/

import { expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

test("SuppressedError", () => {
  const e = expectClean(wire.construct("globalThis.SuppressedError", 1, 2, "m", { cause: 3 }));
  expect([e.error, e.suppressed, e.message]).toEqual([1, 2, "m"]);
});

test("DisposableStack", () => {
  const log = [];
  const s = expectClean(wire.construct("globalThis.DisposableStack"));
  wire.covers("DisposableStack.prototype", s, [
    ["use", [{ [Symbol.dispose]() { log[log.length] = "use"; } }]],
    ["adopt", [1, (v) => { log[log.length] = `adopt ${v}`; }]],
    ["defer", [() => { log[log.length] = "defer"; }]],
    ["disposed", [], false],
  ]);
  const moved = expectClean(wire.invoke("DisposableStack.prototype.move", s));
  wire.covers("DisposableStack.prototype", moved, [["dispose", []], ["[Symbol.dispose]", []]]);
  expect(log).toEqual(["defer", "adopt 1", "use"]);
  const failing = new DisposableStack();
  failing.defer(() => { throw 1; });
  failing.defer(() => { throw 2; });
  const r = wire.invoke("DisposableStack.prototype.dispose", failing);
  expect(r.threw).toBe(true);
  expect(r.hits).toEqual([]);
  expect([r.error.error, r.error.suppressed]).toEqual([1, 2]);
});

test("AsyncDisposableStack", async () => {
  const log = [];
  const s = expectClean(wire.construct("globalThis.AsyncDisposableStack"));
  wire.covers("AsyncDisposableStack.prototype", s, [
    ["use", [{ async [Symbol.asyncDispose]() { log[log.length] = "use"; } }]],
    ["adopt", [1, async (v) => { log[log.length] = `adopt ${v}`; }]],
    ["defer", [() => { log[log.length] = "defer"; }]],
    ["disposed", [], false],
  ]);
  const moved = expectClean(wire.invoke("AsyncDisposableStack.prototype.move", s));
  // each disposal's result is awaited (Await reads a promise's constructor, 27.2.4.7 PromiseResolve)
  expectClean(await wire.invokeAsync("AsyncDisposableStack.prototype.disposeAsync", moved), ["Promise.prototype.constructor"]);
  expectClean(await wire.invokeAsync("AsyncDisposableStack.prototype[Symbol.asyncDispose]", new AsyncDisposableStack()));
  expect(log).toEqual(["defer", "adopt 1", "use"]);
});

test("%AsyncIteratorPrototype%[Symbol.asyncDispose] and the using helpers", async () => {
  const gen = { async *g() { yield 1; } }.g();
  // it looks up and calls the iterator's return (27.1.4.1), and awaits its result
  expectClean(await wire.invokeAsync("%AsyncIteratorPrototype%[Symbol.asyncDispose]", gen), ["%AsyncGeneratorPrototype%.return", "Promise.prototype.constructor"]);
  const log = [];
  const a = { [Symbol.dispose]() { log[log.length] = "a"; } };
  const b = { async [Symbol.asyncDispose]() { log[log.length] = "b"; } };
  const r = await wire.watchAsync(async () => {
    {
      using x = a;
      await using y = b;
    }
  });
  expectClean(r, ["Promise.prototype.constructor"]);
  expect(log).toEqual(["b", "a"]);
});

test("every disposal built-in has a case", () => {
  expect(["DisposableStack.prototype", "AsyncDisposableStack.prototype"].flatMap((o) => wire.uncovered(o))).toEqual([]);
});
