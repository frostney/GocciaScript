/*---
description: Basic object creation and property access works correctly
features: [Object]
---*/

test("basic object creation and property access", () => {
  const obj = { name: "Alice", age: 30, active: true };
  expect(obj.name).toBe("Alice");
  expect(obj.age).toBe(30);
  expect(obj.active).toBeTruthy();
  expect(obj.nonExistent).toBeUndefined();
});
