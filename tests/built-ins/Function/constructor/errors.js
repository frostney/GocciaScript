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

  test("AsyncFunction constructor rejects await in its parameter list", () => {
    const AsyncFunction = Object.getPrototypeOf(async () => {}).constructor;
    expect(() => AsyncFunction("x = await 1", "return x;")).toThrow(SyntaxError);
    expect(() => AsyncFunction("await", "return await;")).toThrow(SyntaxError);
    expect(() => AsyncFunction("...await", "return await.length;")).toThrow(SyntaxError);
  });

  test("GeneratorFunction constructor rejects yield in its parameter list", () => {
    const GeneratorFunction = Object.getPrototypeOf({ *g() {} }.g).constructor;
    expect(() => GeneratorFunction("x = yield 1", "yield x;")).toThrow(SyntaxError);
    expect(() => GeneratorFunction("yield", "yield yield;")).toThrow(SyntaxError);
    expect(() => GeneratorFunction("...yield", "yield yield.length;")).toThrow(SyntaxError);
  });

  test("strict body rejects future reserved identifier references", () => {
    expect(() => new Function('"use strict"; public = 1;')).toThrow(SyntaxError);
  });

  test("strict bodies reject restricted function and parameter bindings", () => {
    expect(() => new Function("eval", '"use strict";')).toThrow(SyntaxError);
    expect(() => new Function("arguments", '"use strict";')).toThrow(SyntaxError);
    expect(() => new Function('function eval() { "use strict"; }')).toThrow(SyntaxError);
    expect(() => new Function('function arguments() { "use strict"; }')).toThrow(SyntaxError);
    expect(() => new Function('"use strict"; const eval = 1;')).toThrow(SyntaxError);
    expect(() => new Function('"use strict"; let arguments = 1;')).toThrow(SyntaxError);
  });

  test("strict bodies reject identifier deletion and unbraced function statements", () => {
    expect(() => new Function('"use strict"; delete value;')).toThrow(SyntaxError);
    expect(() => new Function('"use strict"; if (true) function f() {}')).toThrow(SyntaxError);
    expect(() => new Function("while (false) function f() {}"))
      .toThrow(SyntaxError);
    expect(() => new Function("for (;;) function f() {}"))
      .toThrow(SyntaxError);
  });

  test("expression grammar rejects ambiguous unparenthesized forms", () => {
    expect(() => new Function("return null ?? false || true;"))
      .toThrow(SyntaxError);
    expect(() => new Function("return true && null ?? false;"))
      .toThrow(SyntaxError);
    expect(() => new Function("return -1 ** 2;"))
      .toThrow(SyntaxError);
    expect(() => new Function("let value; ([value]) = [1];"))
      .toThrow(SyntaxError);
  });

  test("optional chains are rejected in forbidden syntactic positions", () => {
    expect(() => new Function("const C = function() {}; return new C?.();"))
      .toThrow(SyntaxError);
    expect(() => new Function("const tag = () => {}; return tag?.`value`;"))
      .toThrow(SyntaxError);
  });

  test("traditional for heads enforce ExpressionNoIn and unique lexical bindings", () => {
    expect(() => new Function("for (let x = 0 in {}; ; ) {}"))
      .toThrow(SyntaxError);
    expect(() => new Function("for (let x = 0, x = 1; ; ) {}"))
      .toThrow(SyntaxError);

    expect(typeof new Function("for (let x = (0 in {}); ; ) { break; }"))
      .toBe("function");
  });

  test("object literals reject invalid shorthand", () => {
    expect(() => new Function("return { true };"))
      .toThrow(SyntaxError);
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
