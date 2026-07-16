/*---
description: WeakSet prototype omits collection enumeration APIs
features: [WeakSet, Symbol.iterator]
---*/

test("does not expose collection enumeration APIs", () => {
  expect(WeakSet.prototype.size).toBe(undefined);
  expect(WeakSet.prototype.clear).toBe(undefined);
  expect(WeakSet.prototype.forEach).toBe(undefined);
  expect(WeakSet.prototype.keys).toBe(undefined);
  expect(WeakSet.prototype.values).toBe(undefined);
  expect(WeakSet.prototype.entries).toBe(undefined);
  expect(WeakSet.prototype[Symbol.iterator]).toBe(undefined);
});
