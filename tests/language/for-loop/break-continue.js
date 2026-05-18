/*---
description: break and continue in traditional for-loop
features: [compat-traditional-for-loop]
---*/

test("break exits", () => {
  const result = [];
  for (let i = 0; i < 10; i++) {
    if (i === 4) break;
    result.push(i);
  }
  expect(result).toEqual([0, 1, 2, 3]);
});

test("continue runs update and skips remainder", () => {
  const result = [];
  for (let i = 0; i < 5; i++) {
    if (i === 2) continue;
    result.push(i);
  }
  expect(result).toEqual([0, 1, 3, 4]);
});

test("continue after closure capture preserves per-iteration binding", () => {
  const fns = [];
  for (let i = 0; i < 3; i++) {
    fns.push(() => i);
    continue;
  }
  expect(fns.map(fn => fn())).toEqual([0, 1, 2]);
});

test("break in nested loop only exits inner", () => {
  const result = [];
  for (let i = 0; i < 3; i++) {
    for (let j = 0; j < 5; j++) {
      if (j === 2) break;
      result.push([i, j]);
    }
  }
  expect(result).toEqual([
    [0, 0], [0, 1],
    [1, 0], [1, 1],
    [2, 0], [2, 1],
  ]);
});

test("continue in nested loop only skips inner", () => {
  const result = [];
  for (let i = 0; i < 3; i++) {
    for (let j = 0; j < 3; j++) {
      if (j === 1) continue;
      result.push([i, j]);
    }
  }
  expect(result).toEqual([
    [0, 0], [0, 2],
    [1, 0], [1, 2],
    [2, 0], [2, 2],
  ]);
});
