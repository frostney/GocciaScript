/*---
description: JSON.stringify converts JavaScript values to JSON strings
features: [JSON.stringify]
---*/

test("JSON.stringify basic values", () => {
  expect(JSON.stringify(42)).toBe("42");
  expect(JSON.stringify("hello")).toBe('"hello"');
  expect(JSON.stringify(true)).toBe("true");
  expect(JSON.stringify(false)).toBe("false");
  expect(JSON.stringify(null)).toBe("null");
  expect(JSON.stringify(undefined)).toBeUndefined();
});

test("JSON.stringify objects", () => {
  const obj = { name: "Alice", age: 30, active: true };
  const json = JSON.stringify(obj);
  expect(json).toBe('{"name":"Alice","age":30,"active":true}');
  expect(json).toContain('"name":"Alice"');
  expect(json).toContain('"age":30');
  expect(json).toContain('"active":true');
});

test("JSON.stringify skips undefined values", () => {
  const obj = {
    name: "Alice",
    age: 30,
    active: true,
    undefinedValue: undefined,
  };
  const json = JSON.stringify(obj);
  expect(json).toBe('{"name":"Alice","age":30,"active":true}');
  expect(json).not.toContain('"undefinedValue":undefined');
});

test("JSON.stringify arrays", () => {
  const arr = [1, "hello", true, null];
  const json = JSON.stringify(arr);
  expect(json).toBe('[1,"hello",true,null]');
  expect(json).toContain("1");
  expect(json).toContain('"hello"');
  expect(json).toContain("true");
  expect(json).toContain("null");
});
