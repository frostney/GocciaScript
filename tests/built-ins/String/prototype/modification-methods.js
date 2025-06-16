/*---
description: String modification methods work correctly
features: [String.prototype.trim, String.prototype.replace, String.prototype.split, String.prototype.repeat]
---*/

test("string modification methods", () => {
  const str = "  hello world  ";
  expect(str.trim()).toBe("hello world");
  expect(str.replace("hello", "hi")).toBe("  hi world  ");
  expect(str.split(" ")).toEqual(["", "", "hello", "world", "", ""]);

  const str2 = "a-b-c";
  expect(str2.split("-")).toEqual(["a", "b", "c"]);
});

test("string repeat method", () => {
  expect("abc".repeat(3)).toBe("abcabcabc");
  expect("x".repeat(0)).toBe("");
  expect("test".repeat(1)).toBe("test");
});

runTests();
