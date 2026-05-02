/*---
description: var supports destructuring patterns
features: [compat-var]
---*/

var __gocciaGlobalDestructuringVar;
({ __gocciaGlobalDestructuringVar } = { __gocciaGlobalDestructuringVar: 73 });

test("var with object destructuring", () => {
  var { a, b } = { a: 1, b: 2 };
  expect(a).toBe(1);
  expect(b).toBe(2);
});

test("var with array destructuring", () => {
  var [x, y] = [10, 20];
  expect(x).toBe(10);
  expect(y).toBe(20);
});

test("top-level destructuring assignment updates global-backed var", () => {
  expect(__gocciaGlobalDestructuringVar).toBe(73);
  expect(globalThis.__gocciaGlobalDestructuringVar).toBe(73);
});
