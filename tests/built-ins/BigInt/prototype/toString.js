/*---
description: BigInt.prototype.toString
features: [bigint]
---*/

test("converts primitive and boxed BigInts to decimal strings", () => {
  expect((42n).toString()).toBe("42");
  expect((-42n).toString()).toBe("-42");
  expect((0n).toString()).toBe("0");
  expect(Object(42n).toString()).toBe("42");
});

test("converts using the requested radix", () => {
  expect((255n).toString(16)).toBe("ff");
  expect((10n).toString(2)).toBe("1010");
  expect((8n).toString(8)).toBe("10");
});

test("rejects non-finite radices", () => {
  expect(() => (5n).toString(Infinity)).toThrow(RangeError);
  expect(() => (5n).toString(NaN)).toThrow(RangeError);
});
