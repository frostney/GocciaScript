/*---
description: A block-bodied arrow function takes no call or member suffix, so a following line that starts with ( or [ is a statement of its own
features: [arrow-function]
---*/

test("a [ line after a block-bodied arrow starts a new statement", () => {
  const seen = [];
  const f = () => {}
  [1, 2].forEach((x) => seen.push(x));
  expect(typeof f).toBe("function");
  expect(seen).toEqual([1, 2]);
});

test("a ( line after a block-bodied arrow starts a new statement", () => {
  let called = false;
  const g = () => { called = true; }
  ("not a call");
  expect(typeof g).toBe("function");
  expect(called).toBe(false);
});

test("a parenthesized arrow can still be called", () => {
  expect((() => { return 6; })()).toBe(6);
});
