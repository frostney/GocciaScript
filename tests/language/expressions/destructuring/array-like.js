/*---
description: Destructuring array-like objects
features: [array-like-destructuring]
---*/

test("destructuring assignment with array-like objects", () => {
  const arrayLike = {
    0: "first",
    1: "second",
    2: "third",
    length: 3,
  };

  let a, b, rest;

  // Cannot destructure array-like as array directly - expect error
  expect(() => {
    [a, b, ...rest] = arrayLike;
  }).toThrow(TypeError);

  // But can destructure as object with numeric keys
  ({ 0: a, 1: b, 2: rest } = arrayLike);
  expect(a).toBe("first");
  expect(b).toBe("second");
  expect(rest).toBe("third");
});
