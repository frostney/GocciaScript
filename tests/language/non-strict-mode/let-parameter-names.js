/*---
description: Non-strict code may use let as a parameter, catch parameter or function name
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

test("let names a catch parameter, a function and a function expression", () => {
  let caught;
  try { throw 7; } catch (let) { caught = let; }
  expect(caught).toBe(7);
  function let() { return 1; }
  expect(let()).toBe(1);
  const f = function let() { return typeof let; };
  expect(f()).toBe("function");
});
