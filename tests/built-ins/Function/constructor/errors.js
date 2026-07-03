/*---
description: Function constructor - error handling
features: [Function, unsafe-function-constructor]
---*/

describe("Function constructor errors", () => {
  test("invalid body throws SyntaxError", () => {
    expect(() => new Function("}{")).toThrow(SyntaxError);
  });

  test("invalid parameter throws SyntaxError", () => {
    expect(() => new Function("@bad", "return 1")).toThrow(SyntaxError);
  });

  test("strict body rejects future reserved identifier references", () => {
    expect(() => new Function('"use strict"; public = 1;')).toThrow(SyntaxError);
  });

  test("super is rejected inside nested dynamic body containers", () => {
    expect(() => new Function("for (; super.value; ) {}")).toThrow(SyntaxError);
    expect(() => new Function("while (super.value) {}")).toThrow(SyntaxError);
    expect(() => new Function("switch (0) { case super.value: break; }")).toThrow(SyntaxError);
    expect(() => new Function("try {} finally { super.value; }")).toThrow(SyntaxError);
    expect(() => new Function("return [super.value];")).toThrow(SyntaxError);
    expect(() => new Function("return { [super.value]: 1 };")).toThrow(SyntaxError);
    expect(() => new Function("return `${super.value}`;")).toThrow(SyntaxError);
  });
});
