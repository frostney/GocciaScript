/*---
description: BigInt.prototype.valueOf
features: [bigint]
---*/

test("returns the primitive value from primitive and boxed receivers", () => {
  expect((42n).valueOf()).toBe(42n);
  expect(Object(42n).valueOf()).toBe(42n);
});

test("rejects incompatible receivers", () => {
  expect(() => BigInt.prototype.valueOf.call(1)).toThrow(TypeError);
  expect(() => BigInt.prototype.valueOf.call({})).toThrow(TypeError);
  expect(() => BigInt.prototype.valueOf.call(null)).toThrow(TypeError);
});
