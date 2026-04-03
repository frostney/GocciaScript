/*---
description: RegExp.prototype.toString
features: [RegExp.prototype.toString]
---*/

test("toString returns literal-like output", () => {
  expect(/abc/gi.toString()).toBe("/abc/gi");
  expect(new RegExp("", "g").toString()).toBe("/(?:)/g");
});
