/*---
description: The async_hooks prelude's AsyncLocalStorage and AsyncResource look up nothing a script can replace
features: [AsyncLocalStorage]
---*/

import { AsyncLocalStorage, AsyncResource } from "node:async_hooks";
import { expectClean, installTripwire } from "./helpers/tripwire.js";

const wire = installTripwire();
afterAll(() => wire.uninstall());

test("AsyncLocalStorage and AsyncResource", () => {
  const P = Object.getPrototypeOf;
  const als = new AsyncLocalStorage({ name: "a", defaultValue: 0 });
  const methods = (o) => Object.getOwnPropertyNames(P(o)).filter((k) => k !== "constructor");
  const r = expectClean(wire.watch(() => {
    const out = [];
    out[0] = als.run(1, (x) => als.getStore() + x, 10);
    out[1] = als.exit(() => als.getStore());
    als.enterWith(2);
    out[2] = als.getStore();
    const snap = AsyncLocalStorage.snapshot();
    out[3] = snap((v) => als.getStore() + v, 5);
    out[4] = AsyncLocalStorage.bind(() => als.getStore())();
    const res = new AsyncResource("t");
    out[5] = res.runInAsyncScope((v) => v + als.getStore(), null, 3);
    out[6] = AsyncResource.bind(() => als.getStore(), "u")();
    out[7] = typeof res.asyncId() === "number" && typeof res.triggerAsyncId() === "number";
    out[8] = res.bind(() => 1)();
    als.disable();
    out[9] = als.getStore();
    return out;
  }), [
    // the test's own calls go through the instances' prototypes
    ...methods(als).map((k) => `AsyncLocalStorage.prototype.${k}`), "AsyncLocalStorage.snapshot", "AsyncLocalStorage.bind",
    ...methods(new AsyncResource("x")).map((k) => `AsyncResource.prototype.${k}`), "AsyncResource.bind",
    "AsyncLocalStorage.prototype.name getter",
  ]);
  // exit runs the callback with no store (Node: getStore() is undefined inside it)
  expect(r).toEqual([11, undefined, 2, 7, 2, 5, 2, true, 1, 0]);
});
