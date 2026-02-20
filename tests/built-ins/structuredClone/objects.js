/*---
description: structuredClone deep-clones objects
features: [structuredClone]
---*/

test("clones a simple object", () => {
  const original = { a: 1, b: "hello", c: true };
  const clone = structuredClone(original);
  expect(clone.a).toBe(1);
  expect(clone.b).toBe("hello");
  expect(clone.c).toBe(true);
});

test("clone is a distinct object", () => {
  const original = { x: 1 };
  const clone = structuredClone(original);
  clone.x = 2;
  expect(original.x).toBe(1);
  expect(clone.x).toBe(2);
});

test("clones nested objects", () => {
  const original = { a: { b: { c: 42 } } };
  const clone = structuredClone(original);
  expect(clone.a.b.c).toBe(42);
  clone.a.b.c = 99;
  expect(original.a.b.c).toBe(42);
});

test("clones object with null prototype value", () => {
  const original = { a: null, b: undefined };
  const clone = structuredClone(original);
  expect(clone.a).toBe(null);
  expect(clone.b).toBe(undefined);
});

test("clones object with mixed value types", () => {
  const original = {
    num: 42,
    str: "hello",
    bool: true,
    nil: null,
    undef: undefined,
    nested: { x: 1 },
    arr: [1, 2, 3],
  };
  const clone = structuredClone(original);
  expect(clone.num).toBe(42);
  expect(clone.str).toBe("hello");
  expect(clone.bool).toBe(true);
  expect(clone.nil).toBe(null);
  expect(clone.undef).toBe(undefined);
  expect(clone.nested.x).toBe(1);
  expect(clone.arr.length).toBe(3);
  expect(clone.arr[0]).toBe(1);
});
