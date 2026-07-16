/*---
description: FinalizationRegistry.prototype.constructor
features: [FinalizationRegistry]
---*/

test("links the prototype and instances back to FinalizationRegistry", () => {
  expect(FinalizationRegistry.prototype.constructor).toBe(FinalizationRegistry);
  expect(new FinalizationRegistry(() => {}).constructor).toBe(FinalizationRegistry);
});
