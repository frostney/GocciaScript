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
