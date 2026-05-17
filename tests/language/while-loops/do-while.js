/*---
description: Basic do...while execution and control flow
features: [compat-while-loops]
---*/

test("do...while executes the body before checking the condition", () => {
  let count = 0;

  do {
    count++;
  } while (false);

  expect(count).toBe(1);
});

test("do...while repeats while condition is true", () => {
  let i = 0;
  let sum = 0;

  do {
    sum += i;
    i++;
  } while (i < 5);

  expect(sum).toBe(10);
  expect(i).toBe(5);
});

test("continue in do...while evaluates the condition", () => {
  const result = [];
  let i = 0;

  do {
    i++;
    if (i === 2) continue;
    result.push(i);
  } while (i < 4);

  expect(result).toEqual([1, 3, 4]);
});

test("break exits a do...while loop", () => {
  const result = [];
  let i = 0;

  do {
    if (i === 3) break;
    result.push(i);
    i++;
  } while (i < 10);

  expect(result).toEqual([0, 1, 2]);
});
