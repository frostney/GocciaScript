/*---
description: String.prototype.substring works correctly
features: [String.prototype.substring]
---*/

test("String.prototype.substring extracts strings", () => {
  const str = "hello world";
  expect(str.substring(0, 5)).toBe("hello");
  expect(str.substring(6)).toBe("world");
  expect(str.substring(0)).toBe("hello world");
  expect(str.substring(0, 0)).toBe("");
  expect(str.substring(0, 1)).toBe("h");
  expect(str.substring(1, 2)).toBe("e");
  expect(str.substring(1, 3)).toBe("el");
  expect(str.substring(1, 4)).toBe("ell");
  expect(str.substring(1, 5)).toBe("ello");
  expect(str.substring(1, 6)).toBe("ello ");
});
