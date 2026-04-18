/*---
description: BigInt prototype methods
features: [bigint]
---*/

test("toString()", () => {
  expect((42n).toString()).toBe("42");
  expect((-42n).toString()).toBe("-42");
  expect((0n).toString()).toBe("0");
});

test("toString(radix)", () => {
  expect((255n).toString(16)).toBe("ff");
  expect((10n).toString(2)).toBe("1010");
  expect((8n).toString(8)).toBe("10");
});

test("valueOf()", () => {
  expect((42n).valueOf()).toBe(42n);
});

test("toLocaleString()", () => {
  expect((42n).toLocaleString()).toBe("42");
});
