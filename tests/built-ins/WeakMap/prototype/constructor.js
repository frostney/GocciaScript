/*---
description: WeakMap.prototype.constructor
features: [WeakMap]
---*/

test("links WeakMap.prototype and WeakMap instances back to WeakMap", () => {
  expect(WeakMap.prototype.constructor).toBe(WeakMap);
  expect(new WeakMap().constructor).toBe(WeakMap);
});
