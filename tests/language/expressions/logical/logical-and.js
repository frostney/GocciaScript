/*---
description: Logical AND (&&) operator works correctly
features: [logical-and-operator]
---*/

test("logical AND (&&)", () => {
  expect(true && true).toBeTruthy();
  expect(true && false).toBeFalsy();
  expect(false && true).toBeFalsy();
  expect(false && false).toBeFalsy();
});

test("logical AND (&&) with short-circuit evaluation", () => {
  let a = 0;
  expect(true && (a = 1)).toBeTruthy();
  expect(a).toBe(1);
});

test("right-nested initializers read mutations between short circuits", () => {
  const run = (p) => {
    let x = 1;
    const result = true && (true && ((x = p) && (true && x)));
    return [result, x];
  };
  expect(run(7)).toEqual([7, 7]);
  expect(run(0)).toEqual([0, 0]);
});
