/*---
description: WeakSet.prototype.constructor
features: [WeakSet]
---*/

test("links WeakSet.prototype and WeakSet instances back to WeakSet", () => {
  expect(WeakSet.prototype.constructor).toBe(WeakSet);
  expect(new WeakSet().constructor).toBe(WeakSet);
});
