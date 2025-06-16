/*---
description: If statement with true condition executes then branch
features: [if-statement]
---*/

test("if statement with true condition", () => {
  let executed = false;
  if (true) {
    executed = true;
  }
  expect(executed).toBeTruthy();
});

runTests();
