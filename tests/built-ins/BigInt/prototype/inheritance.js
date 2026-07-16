/*---
description: BigInt prototype inheritance
features: [bigint]
---*/

test("BigInt.prototype inherits from Object.prototype", () => {
  expect(Object.getPrototypeOf(BigInt.prototype)).toBe(Object.prototype);
});
