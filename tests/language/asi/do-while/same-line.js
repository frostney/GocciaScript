/*---
description: ASI for do...while statement terminators
features: [automatic-semicolon-insertion, compat-while-loops]
---*/

test("do...while ASI terminates before same-line statement", () => {
  let x = 0;
  do { x = 1; } while (false) x = 42;
  expect(x).toBe(42);
});
