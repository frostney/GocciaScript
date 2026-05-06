/*---
description: Increment and decrement operators on BigInt values
features: [bigint]
---*/

test("postfix increment on BigInt variable", () => {
  let i = 10n;
  const old = i++;
  expect(old).toBe(10n);
  expect(i).toBe(11n);
});

test("postfix decrement on BigInt variable", () => {
  let i = 10n;
  const old = i--;
  expect(old).toBe(10n);
  expect(i).toBe(9n);
});

test("prefix increment on BigInt variable", () => {
  let i = 10n;
  const result = ++i;
  expect(result).toBe(11n);
  expect(i).toBe(11n);
});

test("prefix decrement on BigInt variable", () => {
  let i = 10n;
  const result = --i;
  expect(result).toBe(9n);
  expect(i).toBe(9n);
});

test("increment BigInt zero", () => {
  let i = 0n;
  i++;
  expect(i).toBe(1n);
});

test("decrement BigInt zero", () => {
  let i = 0n;
  i--;
  expect(i).toBe(-1n);
});

test("increment negative BigInt", () => {
  let i = -5n;
  i++;
  expect(i).toBe(-4n);
});

test("decrement negative BigInt", () => {
  let i = -1n;
  i--;
  expect(i).toBe(-2n);
});

test("increment large BigInt", () => {
  let i = 9007199254740992n;
  i++;
  expect(i).toBe(9007199254740993n);
});

test("increment on BigInt object property", () => {
  const obj = { count: 5n };
  obj.count++;
  expect(obj.count).toBe(6n);
});

test("decrement on BigInt object property", () => {
  const obj = { count: 5n };
  obj.count--;
  expect(obj.count).toBe(4n);
});

test("prefix increment on BigInt computed property", () => {
  const obj = { x: 3n };
  const key = "x";
  const result = ++obj[key];
  expect(result).toBe(4n);
  expect(obj.x).toBe(4n);
});

test("postfix decrement on BigInt computed property", () => {
  const arr = [10n, 20n, 30n];
  const old = arr[1]--;
  expect(old).toBe(20n);
  expect(arr[1]).toBe(19n);
});
