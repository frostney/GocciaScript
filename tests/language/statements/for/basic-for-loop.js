/*---
description: Basic for loop iterates correctly
features: [for-statement]
---*/

test("basic for loop", () => {
  let sum = 0;
  for (let i = 1; i <= 5; i++) {
    sum += i;
  }
  expect(sum).toBe(15); // 1+2+3+4+5
});
