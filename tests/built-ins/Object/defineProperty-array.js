/*---
description: >
  Object.defineProperty interaction with Array length and numeric indices
features: [Object.defineProperty, Object.getOwnPropertyDescriptor]
---*/

// Issue 2: Array length + defineProperty interaction

test("defineProperty on array length truncates array", () => {
  const arr = [1, 2, 3, 4, 5];
  Object.defineProperty(arr, "length", { value: 2 });
  expect(arr.length).toBe(2);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBe(2);
  expect(arr[2]).toBeUndefined();
});

test("defineProperty on array length extends array", () => {
  const arr = [1, 2];
  Object.defineProperty(arr, "length", { value: 5 });
  expect(arr.length).toBe(5);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBe(2);
  expect(arr[2]).toBeUndefined();
});

test("defineProperty on array length to 0 empties array", () => {
  const arr = [1, 2, 3];
  Object.defineProperty(arr, "length", { value: 0 });
  expect(arr.length).toBe(0);
  expect(arr[0]).toBeUndefined();
});

test("defineProperty on numeric index updates element", () => {
  const arr = [1, 2, 3];
  Object.defineProperty(arr, "0", { value: 99 });
  expect(arr[0]).toBe(99);
  expect(arr.length).toBe(3);
});

test("defineProperty on numeric index beyond length extends array", () => {
  const arr = [1, 2];
  Object.defineProperty(arr, "5", { value: 42 });
  expect(arr[5]).toBe(42);
  expect(arr.length).toBe(6);
  expect(arr[2]).toBeUndefined();
  expect(arr[3]).toBeUndefined();
  expect(arr[4]).toBeUndefined();
});

test("defineProperty on array non-index property", () => {
  const arr = [1, 2, 3];
  Object.defineProperty(arr, "foo", {
    value: "bar",
    writable: true,
    enumerable: true,
    configurable: true,
  });
  expect(arr.foo).toBe("bar");
  expect(arr.length).toBe(3);
});
