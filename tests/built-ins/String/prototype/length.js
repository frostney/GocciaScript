/*---
description: String length property and character access work correctly
features: [String.prototype.length, String.prototype.charAt, String.prototype.charCodeAt]
---*/

test("string length and character access", () => {
  const str = "hello";
  expect(str.length).toBe(5);
  expect(str[0]).toBe("h");
  expect(str[4]).toBe("o");
  expect(str.charAt(1)).toBe("e");
  expect(str.charCodeAt(0)).toBe(104); // 'h'
});
