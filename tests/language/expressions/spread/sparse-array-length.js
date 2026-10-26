/*---
description: Spread of an array whose length is past its stored elements
features: [array-spread, function-spread]
---*/

// Spread runs the array iterator, which reads the array's length at every
// step (ES2026 §23.1.5.2.1 %ArrayIteratorPrototype%.next), so positions past
// the last stored element are spread as undefined.

const farLength = 2000000;

test("array spread copies every index up to a length set far past the elements", () => {
  const array = [1, 2];
  array.length = farLength;
  const copy = [...array];
  expect(copy.length).toBe(farLength);
  expect(copy[1]).toBe(2);
  expect(copy[farLength - 1]).toBe(undefined);
});

test("array spread reads an element stored far past the others", () => {
  const array = [1];
  array[farLength - 1] = "last";
  const copy = [0, ...array];
  expect(copy.length).toBe(farLength + 1);
  expect(copy[farLength]).toBe("last");
});

test("call spread passes every index up to a length set far past the elements", () => {
  const array = [1, 2];
  array.length = farLength;
  const countArguments = (...args) => args.length;
  expect(countArguments(...array)).toBe(farLength);
});
