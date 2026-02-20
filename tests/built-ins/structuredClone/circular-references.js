/*---
description: structuredClone handles circular references
features: [structuredClone]
---*/

test("handles self-referencing object", () => {
  const original = { name: "root" };
  original.self = original;
  const clone = structuredClone(original);
  expect(clone.name).toBe("root");
  expect(clone.self === clone).toBe(true);
  expect(clone !== original).toBe(true);
});

test("handles mutual references between objects", () => {
  const a = { name: "a" };
  const b = { name: "b" };
  a.ref = b;
  b.ref = a;
  const clone = structuredClone(a);
  expect(clone.name).toBe("a");
  expect(clone.ref.name).toBe("b");
  expect(clone.ref.ref === clone).toBe(true);
});

test("handles circular array", () => {
  const arr = [1, 2];
  arr.push(arr);
  const clone = structuredClone(arr);
  expect(clone[0]).toBe(1);
  expect(clone[1]).toBe(2);
  expect(clone[2] === clone).toBe(true);
});

test("preserves shared references within the graph", () => {
  const shared = { x: 1 };
  const original = { a: shared, b: shared };
  const clone = structuredClone(original);
  expect(clone.a.x).toBe(1);
  expect(clone.b.x).toBe(1);
  expect(clone.a === clone.b).toBe(true);
  expect(clone.a !== original.a).toBe(true);
});
