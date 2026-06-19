/*---
description: bound calls to non-strict functions coerce nullish this
features: [non-strict-mode, function-bind]
---*/

test("bound non-strict function receives global this for nullish bound this", () => {
  function captureThis() {
    return this;
  }

  expect(captureThis.bind(null)()).toBe(globalThis);
  expect(captureThis.bind(undefined)()).toBe(globalThis);
});
