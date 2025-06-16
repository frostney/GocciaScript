/*---
description: Basic if statement executes when condition is true
features: [if-statement]
---*/

test("basic if statement", () => {
  let result;
  if (true) {
    result = "executed";
  }
  expect(result).toBe("executed");
});
