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

test("JSON.parse nested objects", () => {
  const obj = JSON.parse('{"a":{"b":1}}');
  expect(obj.a.b).toBe(1);
});

test("JSON.parse empty structures", () => {
  const obj = JSON.parse("{}");
  expect(Object.keys(obj).length).toBe(0);
  const arr = JSON.parse("[]");
  expect(arr.length).toBe(0);
});

test("JSON.parse with escape sequences", () => {
  expect(JSON.parse('"hello\\nworld"')).toBe("hello\nworld");
  expect(JSON.parse('"tab\\there"')).toBe("tab\there");
});

test("JSON.parse throws on invalid JSON", () => {
  expect(() => JSON.parse("undefined")).toThrow(SyntaxError);
  expect(() => JSON.parse("{invalid}")).toThrow(SyntaxError);
  expect(() => JSON.parse("")).toThrow(SyntaxError);
});

test("JSON.parse with reviver transforms values", () => {
  const result = JSON.parse('{"a":1,"b":2}', (key, value) => {
    if (typeof value === "number") {
      return value * 2;
    }
    return value;
  });
  expect(result.a).toBe(2);
  expect(result.b).toBe(4);
});

test("JSON.parse reviver can remove properties", () => {
  const result = JSON.parse('{"a":1,"b":2,"c":3}', (key, value) => {
    if (key === "b") {
      return undefined;
    }
    return value;
  });
  expect(result.a).toBe(1);
  expect(result.c).toBe(3);
  expect(result.b).toBeUndefined();
});

test("JSON.parse reviver receives key and value", () => {
  const keys = [];
  JSON.parse('{"x":10}', (key, value) => {
    keys.push(key);
    return value;
  });
  expect(keys.length).toBe(2);
  expect(keys[0]).toBe("x");
  expect(keys[1]).toBe("");
});

test("JSON.parse reviver works with arrays", () => {
  const result = JSON.parse("[1,2,3]", (key, value) => {
    if (typeof value === "number") {
      return value + 10;
    }
    return value;
  });
  expect(result[0]).toBe(11);
  expect(result[1]).toBe(12);
  expect(result[2]).toBe(13);
});
