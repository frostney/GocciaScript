/*---
description: A var takes an inferred type only from a literal initializer, and a redeclaration keeps the enforced type
features: [types-as-comments, strict-type-enforcement, compat-var]
---*/

describe("var type inference", () => {
  const base = 16;
  const label = () => "text";

  test("literal initializer infers a type", () => {
    var count = 1;
    expect(() => { count = "text"; }).toThrow(TypeError);
    expect(count).toBe(1);
  });

  test("expression initializer remains untyped", () => {
    var count = base + 1;
    count = "4";
    expect(count + 1).toBe("41");
  });

  test("redeclaration that infers no type is checked against the enforced type", () => {
    const redeclare = () => {
      var count = 1;
      var count = label();
      return count + 1;
    };
    expect(redeclare).toThrow(TypeError);
  });

  test("redeclaration with a matching value keeps the enforced type", () => {
    var count = 1;
    var count = base + 1;
    expect(count + 1).toBe(18);
    expect(() => { count = "text"; }).toThrow(TypeError);
  });
});
