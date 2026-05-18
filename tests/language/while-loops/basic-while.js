/*---
description: Basic while-loop execution
features: [compat-while-loops]
---*/

test("while loop repeats while condition is true", () => {
  let i = 0;
  let sum = 0;

  while (i < 5) {
    sum += i;
    i++;
  }

  expect(sum).toBe(10);
  expect(i).toBe(5);
});

test("while loop skips body when condition is initially false", () => {
  let ran = false;

  while (false) {
    ran = true;
  }

  expect(ran).toBe(false);
});

test("condition is evaluated before each iteration", () => {
  let i = 0;
  let checks = 0;

  while ((checks += 1, i < 3)) {
    i++;
  }

  expect(i).toBe(3);
  expect(checks).toBe(4);
});
