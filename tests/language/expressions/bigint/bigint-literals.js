/*---
description: BigInt literal syntax
features: [bigint]
---*/

test("typeof BigInt literals is 'bigint'", () => {
  expect(typeof 0n).toBe("bigint");
  expect(typeof 42n).toBe("bigint");
});

test("BigInt literal value", () => {
  expect(String(42n)).toBe("42");
});

test("large BigInt literal preserves precision beyond MAX_SAFE_INTEGER", () => {
  const big = 9007199254740993n;
  expect(String(big)).toBe("9007199254740993");
});

test("hex BigInt literal", () => {
  expect(String(0xFFn)).toBe("255");
});

test("binary BigInt literal", () => {
  expect(String(0b1010n)).toBe("10");
});

test("octal BigInt literal", () => {
  expect(String(0o17n)).toBe("15");
});

test("zero BigInt literal", () => {
  expect(String(0n)).toBe("0");
});
