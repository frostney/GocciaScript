/*---
description: Basic while loop iterates correctly
features: [while-statement]
---*/

test("basic while loop", () => {
  let i = 0;
  let sum = 0;
  while (i < 5) {
    sum += i;
    i++;
  }
  expect(sum).toBe(10); // 0+1+2+3+4
  expect(i).toBe(5);
});
