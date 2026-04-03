/*---
description: Regular expression literals
features: [RegExp]
---*/

test("regex literals create RegExp objects", () => {
  const regex = /ab/gi;

  expect(regex.source).toBe("ab");
  expect(regex.flags).toBe("gi");
  expect(regex.test("zABz")).toBe(true);
});

test("regex literals coexist with division expressions", () => {
  expect(/ab/.test("zabz")).toBe(true);
  expect(8 / 2).toBe(4);
});

test("regex literals throw SyntaxError for duplicate flags", () => {
  expect(() => {
    /a/gg;
  }).toThrow(SyntaxError);
});
