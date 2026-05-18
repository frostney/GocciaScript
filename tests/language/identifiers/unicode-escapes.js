/*---
description: Identifier unicode escape validation
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

test("identifier unicode escapes reject invalid start characters", () => {
  expect(() => new Function("const \\u0031 = 7;")).toThrow(SyntaxError);
});

test("identifier unicode escapes reject invalid continuation characters", () => {
  expect(() => new Function("const a\\u0020 = 7;")).toThrow(SyntaxError);
});

test("property unicode escapes reject invalid identifier characters", () => {
  expect(() => new Function("const x = { a: 1 }; return x.\\u0020;")).toThrow(SyntaxError);
});
