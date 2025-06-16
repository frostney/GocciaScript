/*---
description: For loops work correctly with different increment patterns and nesting
features: [for-statement]
---*/

test("for loop with different increment", () => {
  let result = [];
  for (let i = 0; i < 10; i += 2) {
    result.push(i);
  }
  expect(result.length).toBe(5);
  expect(result[0]).toBe(0);
  expect(result[4]).toBe(8);
});

test("for loop with decrement", () => {
  let result = [];
  for (let i = 5; i > 0; i--) {
    result.push(i);
  }
  expect(result.length).toBe(5);
  expect(result[0]).toBe(5);
  expect(result[4]).toBe(1);
});

test("nested for loops", () => {
  let count = 0;
  for (let i = 0; i < 3; i++) {
    for (let j = 0; j < 3; j++) {
      count++;
    }
  }
  expect(count).toBe(9);
});
