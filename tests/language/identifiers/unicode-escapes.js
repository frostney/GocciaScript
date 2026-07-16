/*---
description: Valid identifier unicode escapes preserve their identifier names
features: [unicode-identifiers]
---*/

test("identifier unicode escapes preserve valid identifier names", () => {
  const fn = new Function(
    "const \\u0061 = 1; const obj = { a: 2 }; return [a, obj.\\u0061];"
  );
  const result = fn();

  expect(result[0]).toBe(1);
  expect(result[1]).toBe(2);
});

test("escaped let is an identifier reference in non-strict code", () => {
  const fn = new Function("this.let = 4; l\\u0065t\nconst a = 7; return [l\\u0065t, a];");
  const result = fn();

  expect(result[0]).toBe(4);
  expect(result[1]).toBe(7);
});
