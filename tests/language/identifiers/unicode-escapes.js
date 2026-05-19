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

test("identifier unicode escapes reject escaped reserved words", () => {
  expect(() => new Function("const \\u0069f = 7;")).toThrow(SyntaxError);
  expect(() => new Function("return \\u0069f;")).toThrow(SyntaxError);
  expect(() => new Function("const f = (\\u0069f) => 1;")).toThrow(SyntaxError);
  expect(() => new Function("class \\u0069f {}")).toThrow(SyntaxError);
});

test("identifier unicode escapes reject escaped contextual grammar keywords", () => {
  expect(() => new Function("return n\\u0065w.target;")).toThrow(SyntaxError);
  expect(() => new Function("return new.\\u0074arget;")).toThrow(SyntaxError);
  expect(() => new Function("for (let x o\\u0066 [1]) {}")).toThrow(SyntaxError);
  expect(() => new Function("for (let x i\\u006e [1]) {}")).toThrow(SyntaxError);
  expect(() => new Function("return ({ g\\u0065t foo() { return 1; } });")).toThrow(SyntaxError);
});

test("identifier unicode escapes reject escaped expression binding names", () => {
  expect(() => new Function("return function \\u0069f() {};")).toThrow(SyntaxError);
  expect(() => new Function("return function* \\u0069f() {};")).toThrow(SyntaxError);
  expect(() => new Function("return class \\u0069f {};")).toThrow(SyntaxError);
});

test("identifier unicode escapes reject invalid continuation characters", () => {
  expect(() => new Function("const a\\u0020 = 7;")).toThrow(SyntaxError);
});

test("property unicode escapes reject invalid identifier characters", () => {
  expect(() => new Function("const x = { a: 1 }; return x.\\u0020;")).toThrow(SyntaxError);
});
