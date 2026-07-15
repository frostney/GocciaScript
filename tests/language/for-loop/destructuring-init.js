/*---
description: Destructuring patterns in traditional for-init
features: [compat-traditional-for-loop]
---*/

test("array destructuring init", () => {
  const result = [];
  for (let [a, b] = [1, 2]; a < 4; a++, b++) result.push([a, b]);
  expect(result).toEqual([[1, 2], [2, 3], [3, 4]]);
});

test("object destructuring init", () => {
  const result = [];
  for (let { x, y } = { x: 0, y: 10 }; x < 3; x++, y--) result.push([x, y]);
  expect(result).toEqual([[0, 10], [1, 9], [2, 8]]);
});

test("mixed destructuring and simple declarators in init", () => {
  const result = [];
  for (let current = { x: 0, y: 2 }, { x, y } = current; x < 3; x++, y++) {
    result.push([x, y]);
  }
  expect(result).toEqual([[0, 2], [1, 3], [2, 4]]);
});

test("destructuring init creates per-iteration bindings", () => {
  const fns = [];
  for (let [a, b] = [0, 100]; a < 3; a++, b -= 10) fns.push(() => [a, b]);
  expect(fns.map(f => f())).toEqual([[0, 100], [1, 90], [2, 80]]);
});
