/*---
description: BigInt coercion behavior
features: [bigint]
---*/

test("Boolean coercion", () => {
  expect(Boolean(0n)).toBe(false);
  expect(Boolean(1n)).toBe(true);
  expect(Boolean(-1n)).toBe(true);
});

test("string concatenation with +", () => {
  expect("" + 42n).toBe("42");
  expect(42n + "").toBe("42");
  expect("value: " + 42n).toBe("value: 42");
});

test("typeof BigInt", () => {
  expect(typeof 0n).toBe("bigint");
  expect(typeof 42n).toBe("bigint");
});

test("0n is falsy", () => {
  expect(!0n).toBe(true);
});

test("1n is truthy", () => {
  expect(!!1n).toBe(true);
});

test("0n in conditional is falsy", () => {
  let reached = false;
  if (0n) {
    reached = true;
  }
  expect(reached).toBe(false);
});

test("1n in conditional is truthy", () => {
  let reached = false;
  if (1n) {
    reached = true;
  }
  expect(reached).toBe(true);
});
