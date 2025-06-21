/*---
description: String.prototype.slice works correctly
features: [String.prototype.slice]
---*/

test("String.prototype.slice extracts strings", () => {
  const str = "hello world";
  expect(str.slice(0, 5)).toBe("hello");
  expect(str.slice(-5)).toBe("world");
  expect(str.slice(0)).toBe("hello world");
  expect(str.slice(0, 0)).toBe("");
  expect(str.slice(0, 1)).toBe("h");
  expect(str.slice(1, 2)).toBe("e");
  expect(str.slice(1, 3)).toBe("el");
  expect(str.slice(1, 4)).toBe("ell");
  expect(str.slice(1, 5)).toBe("ello");
  expect(str.slice(1, 6)).toBe("ello ");
});
