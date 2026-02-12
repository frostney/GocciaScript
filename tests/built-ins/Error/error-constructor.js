/*---
description: Error constructor creates error objects correctly
features: [Error]
---*/

test("Error constructor", () => {
  const error = new Error("Test error");
  expect(error.message).toBe("Test error");
  expect(error.name).toBe("Error");
});

test("TypeError constructor", () => {
  const error = new TypeError("Type error");
  expect(error.message).toBe("Type error");
  expect(error.name).toBe("TypeError");
});

test("ReferenceError constructor", () => {
  const error = new ReferenceError("Ref error");
  expect(error.message).toBe("Ref error");
  expect(error.name).toBe("ReferenceError");
});
