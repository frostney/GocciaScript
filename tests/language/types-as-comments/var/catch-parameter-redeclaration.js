/*---
description: Under strict types, a var initializer that assigns a redeclared catch parameter is typed as the catch parameter
features: [types-as-comments, strict-type-enforcement, compat-var]
---*/

describe("var redeclaring a catch parameter under strict types", () => {
  test("a number literal initializer does not type the thrown value as a number", () => {
    const add = () => {
      try {
        throw "a";
      } catch (x) {
        var x = 5;
        return x + 1;
      }
    };
    expect(add()).toBe(6);
  });

  test("an annotated initializer is assigned to the catch parameter", () => {
    const add = () => {
      try {
        throw "a";
      } catch (x) {
        var x: number = 5;
        return x * 2;
      }
    };
    expect(add()).toBe(10);
  });

  test("the annotation still rejects an initializer of another type", () => {
    const assign = () => {
      try {
        throw 1;
      } catch (x) {
        var x: number = "text";
      }
    };
    expect(assign).toThrow(TypeError);
  });

  test("the initializer is not checked against the var's enforced type", () => {
    var count: number = 1;
    let inner;
    try {
      throw 0;
    } catch (count) {
      var count = "text";
      inner = count;
    }
    expect(inner).toBe("text");
    expect(count).toBe(1);
  });
});
