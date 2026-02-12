/*---
description: Delete operator removes properties from objects
features: [delete]
---*/

test("delete removes own property", () => {
  const obj = { a: 1, b: 2, c: 3 };
  delete obj.b;
  expect(obj.b).toBeUndefined();
  expect(Object.keys(obj)).toEqual(["a", "c"]);
});

test("delete with bracket notation", () => {
  const obj = { x: 10, y: 20 };
  delete obj["x"];
  expect(obj.x).toBeUndefined();
  expect(Object.keys(obj)).toEqual(["y"]);
});

test("delete non-existent property does not throw", () => {
  const obj = { a: 1 };
  delete obj.nonexistent;
  expect(Object.keys(obj)).toEqual(["a"]);
});

test("delete with computed property name", () => {
  const key = "dynamic";
  const obj = { dynamic: 42, other: 99 };
  delete obj[key];
  expect(obj.dynamic).toBeUndefined();
  expect(obj.other).toBe(99);
});

test("delete array element creates sparse array", () => {
  const arr = [1, 2, 3, 4, 5];
  delete arr[2];
  expect(arr.length).toBe(5);
  expect(arr[2]).toBeUndefined();
  expect(arr[0]).toBe(1);
  expect(arr[4]).toBe(5);
});

test("property is gone after delete", () => {
  const obj = { name: "test", value: 42 };
  expect("value" in obj).toBe(true);
  delete obj.value;
  expect("value" in obj).toBe(false);
});
