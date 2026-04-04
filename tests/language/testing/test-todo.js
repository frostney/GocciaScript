/*---
description: test.todo registers placeholder tests without executing them
features: [Test Assertions]
---*/

test.todo("adds future coverage");

test("continues running non-todo tests", () => {
  expect(true).toBe(true);
});
