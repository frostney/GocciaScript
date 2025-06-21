/*---
description: String.prototype.split works correctly
features: [String.prototype.split]
---*/

test("String.prototype.split splits strings", () => {
  const str = "hello world";
  expect(str.split(" ")).toEqual(["hello", "world"]);
  expect("a-b-c".split("-")).toEqual(["a", "b", "c"]);
  expect(str.split("")).toEqual([
    "h",
    "e",
    "l",
    "l",
    "o",
    " ",
    "w",
    "o",
    "r",
    "l",
    "d",
  ]);
  expect("".split(" ")).toEqual([]);
  expect(" ".split(" ")).toEqual([]);
  expect("  ".split(" ")).toEqual([]);
  expect("  hello world  ".split(" ")).toEqual([
    "",
    "",
    "hello",
    "world",
    "",
    "",
  ]);
});
