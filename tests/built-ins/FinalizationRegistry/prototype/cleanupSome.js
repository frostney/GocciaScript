/*---
description: FinalizationRegistry.prototype.cleanupSome proposal surface
features: [FinalizationRegistry]
---*/

test("is not exposed", () => {
  expect(FinalizationRegistry.prototype.cleanupSome).toBe(undefined);
});
