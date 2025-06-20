/*---
description: Error cases
features: [destructuring]
---*/

test("destructuring assignment with null and undefined", () => {
  let x, y, z;

  // Null destructuring should throw TypeError
  expect(() => {
    ({ x } = null);
  }).toThrow(TypeError);

  expect(() => {
    ({ y } = undefined);
  }).toThrow(TypeError);

  // Array destructuring with null/undefined should throw
  expect(() => {
    [z] = null;
  }).toThrow(TypeError);

  expect(() => {
    [z] = undefined;
  }).toThrow(TypeError);
});
