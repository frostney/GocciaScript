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
