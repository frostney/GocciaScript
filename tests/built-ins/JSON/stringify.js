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
});

test("JSON.stringify undefined in array becomes null", () => {
  expect(JSON.stringify([1, undefined, 3])).toBe("[1,null,3]");
});

test("JSON.stringify nested objects", () => {
  const obj = { a: { b: { c: 1 } } };
  expect(JSON.stringify(obj)).toBe('{"a":{"b":{"c":1}}}');
});

test("JSON.stringify empty structures", () => {
  expect(JSON.stringify({})).toBe("{}");
  expect(JSON.stringify([])).toBe("[]");
});

test("JSON.stringify NaN and Infinity become null", () => {
  expect(JSON.stringify(NaN)).toBe("null");
  expect(JSON.stringify(Infinity)).toBe("null");
  expect(JSON.stringify(-Infinity)).toBe("null");
});

test("JSON.stringify strings with special characters", () => {
  expect(JSON.stringify("hello\nworld")).toBe('"hello\\nworld"');
  expect(JSON.stringify("tab\there")).toBe('"tab\\there"');
});

test("JSON.stringify with space number", () => {
  const obj = { a: 1 };
  const result = JSON.stringify(obj, null, 2);
  expect(result).toContain("\n");
  expect(result).toContain("  ");
  expect(result).toContain('"a"');
});

test("JSON.stringify with space string", () => {
  const obj = { a: 1 };
  const result = JSON.stringify(obj, null, "\t");
  expect(result).toContain("\n");
  expect(result).toContain("\t");
});

test("JSON.stringify with replacer function", () => {
  const obj = { a: 1, b: "hello", c: true };
  const result = JSON.stringify(obj, (key, value) => {
    if (typeof value === "number") {
      return value * 2;
    }
    return value;
  });
  const parsed = JSON.parse(result);
  expect(parsed.a).toBe(2);
  expect(parsed.b).toBe("hello");
});

test("JSON.stringify replacer function can exclude properties", () => {
  const obj = { a: 1, b: 2, c: 3 };
  const result = JSON.stringify(obj, (key, value) => {
    if (key === "b") {
      return undefined;
    }
    return value;
  });
  const parsed = JSON.parse(result);
  expect(parsed.a).toBe(1);
  expect(parsed.c).toBe(3);
  expect(parsed.b).toBeUndefined();
});

test("JSON.stringify with array replacer", () => {
  const obj = { a: 1, b: 2, c: 3 };
  const result = JSON.stringify(obj, ["a", "c"]);
  const parsed = JSON.parse(result);
  expect(parsed.a).toBe(1);
  expect(parsed.c).toBe(3);
  expect(parsed.b).toBeUndefined();
});

test("JSON.stringify space is capped at 10", () => {
  const obj = { a: 1 };
  const result = JSON.stringify(obj, null, 20);
  const lines = result.split("\n");
  expect(lines.length).toBe(3);
});
