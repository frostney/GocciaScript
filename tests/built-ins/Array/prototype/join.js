/*---
description: Array.prototype.join joins the elements of an array into a string
features: [Array.prototype.join]
---*/

test("Array.prototype.join joins the elements of an array into a string", () => {
  const arr = [1, 2, 3];
  const joined = arr.join(",");
  expect(joined).toBe("1,2,3");
});

test("Array.prototype.join with custom separator", () => {
  const arr = [1, 2, 3];
  const joined = arr.join("-");
  expect(joined).toBe("1-2-3");
});

test("Array.prototype.join with empty array", () => {
  const arr = [];
  const joined = arr.join(",");
  expect(joined).toBe("");
});
