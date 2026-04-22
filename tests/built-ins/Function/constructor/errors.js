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
});
