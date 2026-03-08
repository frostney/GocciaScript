/*---
description: Decrement operator works correctly
features: [decrement-operator]
---*/

test("pre-decrement", () => {
  let a = 5;
  expect(--a).toBe(4);
  expect(a).toBe(4);
});

test("post-decrement", () => {
  let a = 5;
  expect(a--).toBe(5);
  expect(a).toBe(4);
});

test("decrement preserves fractional part", () => {
  let x = 2.5;
  x--;
  expect(x).toBe(1.5);

  let y = 0.5;
  --y;
  expect(y).toBe(-0.5);
});

test("post-decrement on property access", () => {
  const obj = { count: 10 };
  expect(obj.count--).toBe(10);
  expect(obj.count).toBe(9);
});

test("pre-decrement on property access", () => {
  const obj = { count: 10 };
  expect(--obj.count).toBe(9);
  expect(obj.count).toBe(9);
});

test("post-decrement on computed member (array index)", () => {
  const arr = [5, 10, 15];
  expect(arr[0]--).toBe(5);
  expect(arr[0]).toBe(4);
  expect(arr[2]--).toBe(15);
  expect(arr[2]).toBe(14);
});

test("pre-decrement on computed member (array index)", () => {
  const arr = [5, 10, 15];
  expect(--arr[0]).toBe(4);
  expect(arr[0]).toBe(4);
  expect(--arr[1]).toBe(9);
  expect(arr[1]).toBe(9);
});

test("decrement on computed member with variable key", () => {
  const arr = [10, 20, 30];
  let i = 2;
  arr[i]--;
  expect(arr[2]).toBe(29);
});

test("decrement on computed member with string key", () => {
  const obj = { x: 100, y: 200 };
  const key = "x";
  obj[key]--;
  expect(obj.x).toBe(99);
  --obj["y"];
  expect(obj.y).toBe(199);
});

test("decrement on computed member with symbol key", () => {
  const sym = Symbol("counter");
  const obj = { [sym]: 10 };
  obj[sym]--;
  expect(obj[sym]).toBe(9);
  --obj[sym];
  expect(obj[sym]).toBe(8);
});

test("post-decrement on non-writable property throws", () => {
  const obj = {};
  Object.defineProperty(obj, "x", { value: 5, writable: false });
  expect(() => obj.x--).toThrow(TypeError);
  expect(obj.x).toBe(5);
});
