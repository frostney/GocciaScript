/*---
description: Increment operator works correctly
features: [increment-operators]
---*/

test("pre-increment", () => {
  let a = 5;
  expect(a).toBe(5);
  expect(++a).toBe(6);
  expect(a).toBe(6);
});

test("post-increment", () => {
  let a = 5;
  expect(a).toBe(5);
  expect(a++).toBe(5);
  expect(a).toBe(6);
});

test("increment preserves fractional part", () => {
  let x = 1.5;
  x++;
  expect(x).toBe(2.5);

  let y = -0.5;
  ++y;
  expect(y).toBe(0.5);
});

test("post-increment on property access", () => {
  const obj = { count: 10 };
  expect(obj.count++).toBe(10);
  expect(obj.count).toBe(11);
});

test("pre-increment on property access", () => {
  const obj = { count: 10 };
  expect(++obj.count).toBe(11);
  expect(obj.count).toBe(11);
});

test("post-increment on computed member (array index)", () => {
  const arr = [1, 2, 3];
  expect(arr[0]++).toBe(1);
  expect(arr[0]).toBe(2);
  expect(arr[1]++).toBe(2);
  expect(arr[1]).toBe(3);
});

test("pre-increment on computed member (array index)", () => {
  const arr = [1, 2, 3];
  expect(++arr[0]).toBe(2);
  expect(arr[0]).toBe(2);
  expect(++arr[2]).toBe(4);
  expect(arr[2]).toBe(4);
});

test("increment on computed member with variable key", () => {
  const arr = [10, 20, 30];
  let i = 1;
  arr[i]++;
  expect(arr[1]).toBe(21);
});

test("increment on computed member with string key", () => {
  const obj = { a: 5, b: 10 };
  const key = "a";
  obj[key]++;
  expect(obj.a).toBe(6);
  ++obj["b"];
  expect(obj.b).toBe(11);
});
