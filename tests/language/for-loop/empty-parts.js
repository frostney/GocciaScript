/*---
description: Traditional for-loop with omitted init/condition/update parts
features: [compat-traditional-for-loop]
---*/

test("empty condition is treated as true (terminated by break)", () => {
  const result = [];
  let i = 0;
  for (let _ = 0;; _++) {
    if (i >= 3) break;
    result.push(i);
    i++;
  }
  expect(result).toEqual([0, 1, 2]);
});

test("omitted update", () => {
  const result = [];
  for (let i = 0; i < 3;) {
    result.push(i);
    i++;
  }
  expect(result).toEqual([0, 1, 2]);
});

test("omitted init", () => {
  const result = [];
  let i = 0;
  for (; i < 3; i++) result.push(i);
  expect(result).toEqual([0, 1, 2]);
});

test("for(;;) infinite loop with break", () => {
  const result = [];
  let i = 0;
  for (;;) {
    if (i >= 4) break;
    result.push(i);
    i++;
  }
  expect(result).toEqual([0, 1, 2, 3]);
});

test("expression-statement init", () => {
  const result = [];
  let i;
  for (i = 0; i < 3; i++) result.push(i);
  expect(result).toEqual([0, 1, 2]);
  expect(i).toBe(3);
});
