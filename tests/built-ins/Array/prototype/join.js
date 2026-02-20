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

test("join with no separator defaults to comma", () => {
  expect([1, 2, 3].join()).toBe("1,2,3");
});

test("join with undefined separator defaults to comma", () => {
  expect([1, 2, 3].join(undefined)).toBe("1,2,3");
});

test("join with empty string separator", () => {
  expect([1, 2, 3].join("")).toBe("123");
});

test("join converts null and undefined to empty string", () => {
  expect([1, null, undefined, 4].join(",")).toBe("1,,,4");
});

test("join with single element (no separator)", () => {
  expect([42].join(",")).toBe("42");
});

test("join converts elements to strings", () => {
  expect([true, false, 0].join(",")).toBe("true,false,0");
});
