/*---
description: ShadowRealm.prototype.constructor
features: [ShadowRealm]
---*/

test("points back to ShadowRealm", () => {
  expect(ShadowRealm.prototype.constructor).toBe(ShadowRealm);
});
