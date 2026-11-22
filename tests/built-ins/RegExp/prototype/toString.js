/*---
description: RegExp.prototype.toString
features: [RegExp.prototype.toString]
---*/

test("toString returns literal-like output", () => {
  expect(/abc/gi.toString()).toBe("/abc/gi");
  expect(new RegExp("", "g").toString()).toBe("/(?:)/g");
  expect(new RegExp("/", "").toString()).toBe("/\\//");
  expect(new RegExp("\n", "").toString()).toBe("/\\n/");
});

test("toString keeps the one backslash of an escaped slash", () => {
  expect(/<\/a>/y.toString()).toBe("/<\\/a>/y");
  expect(/a\/b/.toString()).toBe("/a\\/b/");
});

test("toString leaves a slash inside a character class unescaped", () => {
  expect(/[/]/.toString()).toBe("/[/]/");
});

test("toString keeps an escaped slash that follows an escaped backslash", () => {
  expect(/\\\//.toString()).toBe("/\\\\\\//");
});

test("toString escapes an unescaped slash that follows an escaped backslash once", () => {
  expect(new RegExp("\\\\/").toString()).toBe("/\\\\\\//");
});

test("toString escapes a slash after an escaped bracket, which opens no class", () => {
  expect(new RegExp("\\[/]").toString()).toBe("/\\[\\/]/");
});

test("toString is generic for object receivers", () => {
  expect(RegExp.prototype.toString.call({ source: "abc", flags: "g" })).toBe("/abc/g");
  expect(RegExp.prototype.toString.call({})).toBe("/undefined/undefined");
  expect(() => RegExp.prototype.toString.call(1)).toThrow(TypeError);
});

test("toString reads source and flags properties from RegExp receivers", () => {
  const regex = /abc/g;

  Object.defineProperty(regex, "source", { value: "override" });
  Object.defineProperty(regex, "flags", { value: "i" });

  expect(regex.toString()).toBe("/override/i");
});
