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

test("Object.entries returns an array of [key, value] pairs for an object with a prototype", () => {
  const obj = Object.create({ a: 1, b: 2, c: 3 });
  expect(Object.entries(obj)).toEqual([
    ["a", 1],
    ["b", 2],
    ["c", 3],
  ]);
});
