/*---
description: Non-strict functions may name a parameter let
features: [compat-function, compat-non-strict-mode]
---*/

test("let names a plain and a rest parameter of a non-strict function", () => {
  function plain(let) { return let + 1; }
  function rest(x, ...let) { return let.length + x; }
  expect(plain(1)).toBe(2);
  expect(rest(1, 2, 3)).toBe(3);
});

test("let names a rest parameter of a non-strict arrow", () => {
  const f = (...let) => let.length;
  expect(f(1, 2)).toBe(2);
});
