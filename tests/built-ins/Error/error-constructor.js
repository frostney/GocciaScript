/*---
description: Error constructor creates error objects correctly
features: [Error]
---*/

test("Error constructor", () => {
  const error = new Error("Test error");
  expect(error.message).toBe("Test error");
  expect(error.name).toBe("Error");
  expect(error instanceof Error).toBeTruthy();
});

runTests();
