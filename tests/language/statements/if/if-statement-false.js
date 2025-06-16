/*---
description: If statement with false condition does not execute then branch
features: [if-statement]
---*/

test("if statement with false condition", () => {
  let executed = false;
  if (false) {
    executed = true;
  }
  expect(executed).toBeFalsy();
});

runTests();
