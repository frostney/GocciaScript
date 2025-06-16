/*---
description: String concatenation with + operator works correctly
features: [string-concatenation]
---*/

test("string concatenation", () => {
  expect("Hello" + " " + "World").toBe("Hello World");
  expect("Number: " + 42).toBe("Number: 42");
  expect("" + true).toBe("true");
  expect("Value: " + null).toBe("Value: null");
});
