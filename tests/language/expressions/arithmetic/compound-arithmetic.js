/*---
description: Compound assignment operators work correctly
features: [compound-assignment-operators]
---*/

test("compound assignment operators", () => {
  let a = 5;
  a += 3;
  expect(a).toBe(8);
});

test("compound assignment operators with string", () => {
  let a = "hello";
  a += " world";
  expect(a).toBe("hello world");
});

test("compound assignment operators with number", () => {
  let a = 5;
  a *= 2;
  expect(a).toBe(10);
});

test("compound assignment operators with boolean", () => {
  let a = true;
  a &= false;
  expect(a).toBe(0);
});

test("compound assignment operators with null", () => {
  let a = null;
  a += 3;
  expect(a).toBe(3);
});

test("compound assignment operators with undefined", () => {
  let a = undefined;
  a += 3;
  expect(a).toBe(NaN);
});

test("compound assignment operators with object", () => {
  let a = { value: 5 };
  a.value += 3;
  expect(a.value).toBe(8);
});

test("compound assignment operators with array", () => {
  let a = [1, 2, 3];
  a[0] += 3;
  expect(a[0]).toBe(4);
});

test("compound division", () => {
  let a = 10;
  a /= 2;
  expect(a).toBe(5);
});

test("compound modulo", () => {
  let a = 10;
  a %= 3;
  expect(a).toBe(1);
});

test("compound exponentiation", () => {
  let a = 2;
  a **= 3;
  expect(a).toBe(8);
});
