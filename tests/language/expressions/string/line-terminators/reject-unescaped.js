/*---
description: String literals reject unescaped line terminators (ES2026 12.9.4)
features: [string-literals, unsafe-function-constructor]
---*/

test("unescaped LF in string literal throws SyntaxError", () => {
  expect(() => new Function('"hello\nworld"')).toThrow(SyntaxError);
  expect(() => new Function("'hello\nworld'")).toThrow(SyntaxError);
});

test("unescaped CR in string literal throws SyntaxError", () => {
  expect(() => new Function('"hello\rworld"')).toThrow(SyntaxError);
  expect(() => new Function("'hello\rworld'")).toThrow(SyntaxError);
});

test("unescaped LS in string literal throws SyntaxError", () => {
  const ls = String.fromCharCode(0x2028);
  expect(() => new Function('"hello' + ls + 'world"')).toThrow(SyntaxError);
  expect(() => new Function("'hello" + ls + "world'")).toThrow(SyntaxError);
});

test("unescaped PS in string literal throws SyntaxError", () => {
  const ps = String.fromCharCode(0x2029);
  expect(() => new Function('"hello' + ps + 'world"')).toThrow(SyntaxError);
  expect(() => new Function("'hello" + ps + "world'")).toThrow(SyntaxError);
});

test("escaped line terminators in string literals are valid", () => {
  const f1 = new Function('return "hello\\nworld"');
  expect(f1()).toBe("hello\nworld");
  const f2 = new Function('return "hello\\rworld"');
  expect(f2()).toBe("hello\rworld");
  const f3 = new Function('return "hello\\u2028world"');
  expect(f3()).toBe("hello" + String.fromCharCode(0x2028) + "world");
  const f4 = new Function('return "hello\\u2029world"');
  expect(f4()).toBe("hello" + String.fromCharCode(0x2029) + "world");
});

test("line continuation in source file is still valid", () => {
  expect("hello\
world").toBe("helloworld");
  expect('hello\
world').toBe("helloworld");
});
