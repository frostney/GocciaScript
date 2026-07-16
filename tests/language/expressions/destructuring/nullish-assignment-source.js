/*---
description: Destructuring assignment rejects nullish sources
features: [destructuring]
---*/

test("object destructuring assignment rejects null and undefined", () => {
  let value;

  expect(() => {
    ({ value } = null);
  }).toThrow(TypeError);

  expect(() => {
    ({ value } = undefined);
  }).toThrow(TypeError);
});

test("array destructuring assignment rejects null and undefined", () => {
  let value;

  expect(() => {
    [value] = null;
  }).toThrow(TypeError);

  expect(() => {
    [value] = undefined;
  }).toThrow(TypeError);
});
