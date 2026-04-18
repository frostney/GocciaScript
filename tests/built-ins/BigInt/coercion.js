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

test("String() explicit conversion", () => {
  expect(String(42n)).toBe("42");
  expect(String(0n)).toBe("0");
  expect(String(-1n)).toBe("-1");
});

test("Number() throws TypeError for BigInt", () => {
  expect(() => { Number(1n); }).toThrow();
});

test("Object() boxing", () => {
  const boxed = Object(1n);
  expect(typeof boxed).toBe("object");
  expect(boxed.valueOf() === 1n).toBe(true);
  expect(boxed.toString()).toBe("1");
});

test("Object() boxed BigInt in arithmetic", () => {
  expect(Object(1n) - 1n === 0n).toBe(true);
  expect(Object(2n) * 3n === 6n).toBe(true);
});
