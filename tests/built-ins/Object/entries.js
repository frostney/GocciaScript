/*---
features: [Object.entries]
---*/

test("Object.entries returns an array of [key, value] pairs", () => {
  const obj = { a: 1, b: 2, c: 3 };
  expect(Object.entries(obj)).toEqual([
    ["a", 1],
    ["b", 2],
    ["c", 3],
  ]);
});

test("Object.entries returns an empty array for an empty object", () => {
  const obj = {};
  expect(Object.entries(obj)).toEqual([]);
});

test("Object.entries does not return an array of [key, value] pairs for its prototype properties", () => {
  const obj = Object.create({ a: 1, b: 2, c: 3 });
  // Object.entries only returns own enumerable properties, not prototype properties
  expect(Object.entries(obj)).toEqual([]);

  // If we want to test with own properties, add them explicitly
  obj.x = 4;
  obj.y = 5;

  expect(Object.entries(obj)).toEqual([
    ["x", 4],
    ["y", 5],
  ]);
});
