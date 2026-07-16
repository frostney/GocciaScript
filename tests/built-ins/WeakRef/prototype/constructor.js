/*---
description: WeakRef.prototype.constructor
features: [WeakRef]
---*/

test("links WeakRef.prototype and WeakRef instances back to WeakRef", () => {
  expect(WeakRef.prototype.constructor).toBe(WeakRef);
  expect(new WeakRef({}).constructor).toBe(WeakRef);
});
