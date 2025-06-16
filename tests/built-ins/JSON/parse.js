/*---
description: JSON.parse converts JSON strings to JavaScript values
features: [JSON.parse]
---*/

test("JSON.parse basic values", () => {
  expect(JSON.parse("42")).toBe(42);
  expect(JSON.parse('"hello"')).toBe("hello");
  expect(JSON.parse("true")).toBeTruthy();
  expect(JSON.parse("false")).toBeFalsy();
  expect(JSON.parse("null")).toBeNull();
});

test("JSON.parse objects", () => {
  const json = '{"name":"Alice","age":30,"active":true}';
  const obj = JSON.parse(json);
  expect(obj.name).toBe("Alice");
  expect(obj.age).toBe(30);
  expect(obj.active).toBeTruthy();
});

test("JSON.parse arrays", () => {
  const json = '[1,"hello",true,null]';
  const arr = JSON.parse(json);
  expect(arr.length).toBe(4);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBe("hello");
  expect(arr[2]).toBeTruthy();
  expect(arr[3]).toBeNull();
});

runTests();
