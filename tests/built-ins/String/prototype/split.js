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
  expect("  hello world  ".split(" ")).toEqual([
    "",
    "",
    "hello",
    "world",
    "",
    "",
  ]);
});

test("String.prototype.split with spaces and empty strings", () => {
  const str = "  hello world  ";
  expect(str.split(" ")).toEqual(["", "", "hello", "world", "", ""]);
  expect("".split("")).toEqual([]);
  expect("".split(" ")).toEqual([""]);
  expect(" ".split("")).toEqual([" "]);
  expect("  ".split("")).toEqual([" ", " "]);
});

test("String.prototype.split with limits", () => {
  expect("a,b,c,d".split(",", 2)).toEqual(["a", "b"]);
  expect("a,b,c,d".split(",", 0)).toEqual([]);
  expect("hello world".split("", 3)).toEqual(["h", "e", "l"]);
  expect("one-two-three".split("-", 1)).toEqual(["one"]);
});

test("String.prototype.split edge cases", () => {
  expect("hello".split(null)).toEqual(["hello"]);
  expect("hello".split(undefined)).toEqual(["hello"]);
  expect("hello".split(0)).toEqual(["hello"]);
  expect("hello".split(2)).toEqual(["hello"]);
  expect("hello".split(NaN)).toEqual(["hello"]);
  expect("hello".split(Infinity)).toEqual(["hello"]);
  expect("hello".split(-Infinity)).toEqual(["hello"]);
  expect("hello".split(true)).toEqual(["hello"]);
  expect("hello".split(false)).toEqual(["hello"]);
  expect("hello".split({})).toEqual(["hello"]);
  expect("hello".split([])).toEqual(["h", "e", "l", "l", "o"]);
});

test("String.prototype.split with limits and edge cases", () => {
  expect("".split("", 5)).toEqual([]);
  expect("abc".split("", 0)).toEqual([]);
  expect("a,b,c".split(",", 10)).toEqual(["a", "b", "c"]);
});
