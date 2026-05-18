/*---
description: break and continue in while loops
features: [compat-while-loops]
---*/

test("break exits a while loop", () => {
  const result = [];
  let i = 0;

  while (i < 10) {
    if (i === 4) break;
    result.push(i);
    i++;
  }

  expect(result).toEqual([0, 1, 2, 3]);
});

test("continue rechecks the while condition", () => {
  const result = [];
  let i = 0;

  while (i < 5) {
    i++;
    if (i === 3) continue;
    result.push(i);
  }

  expect(result).toEqual([1, 2, 4, 5]);
});

test("break in nested while loop exits only the inner loop", () => {
  const result = [];
  let i = 0;

  while (i < 3) {
    let j = 0;
    while (j < 5) {
      if (j === 2) break;
      result.push([i, j]);
      j++;
    }
    i++;
  }

  expect(result).toEqual([
    [0, 0], [0, 1],
    [1, 0], [1, 1],
    [2, 0], [2, 1],
  ]);
});
