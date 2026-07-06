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
