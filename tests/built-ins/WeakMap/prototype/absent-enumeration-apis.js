/*---
description: WeakMap prototype omits collection enumeration APIs
features: [WeakMap, Symbol.iterator]
---*/

test("does not expose collection enumeration APIs", () => {
  expect(WeakMap.prototype.size).toBe(undefined);
  expect(WeakMap.prototype.clear).toBe(undefined);
  expect(WeakMap.prototype.forEach).toBe(undefined);
  expect(WeakMap.prototype.keys).toBe(undefined);
  expect(WeakMap.prototype.values).toBe(undefined);
  expect(WeakMap.prototype.entries).toBe(undefined);
  expect(WeakMap.prototype[Symbol.iterator]).toBe(undefined);
});
