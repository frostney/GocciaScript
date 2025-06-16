/*---
description: Break and continue statements work correctly in loops
features: [break-statement, continue-statement]
---*/

test("break statement in for loop", () => {
  let result = [];
  for (let i = 0; i < 10; i++) {
    if (i === 5) break;
    result.push(i);
  }
  expect(result.length).toBe(5);
  expect(result).toEqual([0, 1, 2, 3, 4]);
});

test("continue statement in for loop", () => {
  let result = [];
  for (let i = 0; i < 5; i++) {
    if (i === 2) continue;
    result.push(i);
  }
  expect(result.length).toBe(4);
  expect(result).toEqual([0, 1, 3, 4]);
});

test("break in while loop", () => {
  let i = 0;
  let values = [];
  while (true) {
    if (i >= 3) break;
    values.push(i);
    i++;
  }
  expect(values).toEqual([0, 1, 2]);
});

test("continue in while loop", () => {
  let i = 0;
  let values = [];
  while (i < 5) {
    i++;
    if (i === 3) continue;
    values.push(i);
  }
  expect(values).toEqual([1, 2, 4, 5]);
});
