/*---
description: break and continue in for-in loops
features: [compat-for-in-loop]
---*/

test("break exits the loop", () => {
  const keys = [];
  for (const key in { a: 1, b: 2, c: 3 }) {
    if (key === "c") break;
    keys.push(key);
  }
  expect(keys).toEqual(["a", "b"]);
});

test("continue skips the rest of the body", () => {
  const keys = [];
  for (const key in { a: 1, b: 2, c: 3 }) {
    if (key === "b") continue;
    keys.push(key);
  }
  expect(keys).toEqual(["a", "c"]);
});

test("closures capture per-iteration lexical bindings", () => {
  const fns = [];
  for (const key in { a: 1, b: 2, c: 3 }) {
    fns.push(() => key);
  }
  expect(fns.map(fn => fn())).toEqual(["a", "b", "c"]);
});
