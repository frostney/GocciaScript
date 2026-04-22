/*---
description: Function constructor throws when flag is not set
features: [Function]
---*/

describe("Function constructor disabled", () => {
  test("new Function() throws TypeError when disabled", () => {
    expect(() => new Function("return 1")).toThrow();
  });

  test("Function() throws TypeError when disabled", () => {
    expect(() => Function("return 1")).toThrow();
  });
});
