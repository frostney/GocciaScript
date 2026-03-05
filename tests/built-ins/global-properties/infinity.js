/*---
description: >
  Infinity is a global property (not a keyword) per ECMAScript spec.
  It can be shadowed by local variable declarations.
features: [global-properties, Infinity, shadowing]
---*/

describe("Infinity as a global property", () => {
  test("global Infinity has the correct value", () => {
    expect(Infinity).toBe(Infinity);
    expect(Infinity > 0).toBe(true);
    expect(Infinity > 999999999).toBe(true);
  });

  test("typeof Infinity returns 'number'", () => {
    expect(typeof Infinity).toBe("number");
  });

  test("Infinity is equal to itself", () => {
    expect(Infinity === Infinity).toBe(true);
  });

  test("negative Infinity", () => {
    expect(-Infinity === -Infinity).toBe(true);
    expect(-Infinity < 0).toBe(true);
    expect(-Infinity < Infinity).toBe(true);
    expect(Infinity > -Infinity).toBe(true);
  });

  test("Infinity is truthy", () => {
    let result = "falsy";
    if (Infinity) {
      result = "truthy";
    }
    expect(result).toBe("truthy");
  });

  test("negative Infinity is truthy", () => {
    let result = "falsy";
    if (-Infinity) {
      result = "truthy";
    }
    expect(result).toBe("truthy");
  });

  test("arithmetic with Infinity", () => {
    expect(Infinity + 1).toBe(Infinity);
    expect(Infinity + Infinity).toBe(Infinity);
    expect(Infinity * 2).toBe(Infinity);
    expect(Infinity * -1).toBe(-Infinity);
    expect(1 / 0).toBe(Infinity);
    expect(-1 / 0).toBe(-Infinity);
    expect(Number.isNaN(Infinity - Infinity)).toBe(true);
    expect(Number.isNaN(Infinity / Infinity)).toBe(true);
    expect(Number.isNaN(Infinity % 1)).toBe(true);
  });

  test("Infinity converts to 'Infinity' string", () => {
    expect(`${Infinity}`).toBe("Infinity");
    expect("" + Infinity).toBe("Infinity");
  });

  test("Number.isFinite rejects Infinity", () => {
    expect(Number.isFinite(Infinity)).toBe(false);
    expect(Number.isFinite(-Infinity)).toBe(false);
  });
});

describe("Infinity immutability", () => {
  test("global Infinity cannot be reassigned", () => {
    expect(() => {
      Infinity = 42;
    }).toThrow(TypeError);
  });

  test("global Infinity remains unchanged after failed reassignment", () => {
    try {
      Infinity = 0;
    } catch (e) {
      // expected
    }
    expect(Infinity).toBe(Infinity);
  });
});

describe("Infinity shadowing", () => {
  test("Infinity can be shadowed in a block scope with let", () => {
    expect(Infinity > 0).toBe(true);
    {
      let Infinity = 42;
      expect(Infinity).toBe(42);
    }
    expect(Infinity > 0).toBe(true);
  });

  test("Infinity can be shadowed in a block scope with const", () => {
    {
      const Infinity = "finite";
      expect(Infinity).toBe("finite");
    }
    expect(Number.isFinite(Infinity)).toBe(false);
  });

  test("shadowed Infinity does not affect outer scope", () => {
    const outer = Infinity;
    {
      let Infinity = 100;
      expect(Infinity).toBe(100);
    }
    expect(Number.isFinite(outer)).toBe(false);
  });

  test("nested shadowing restores correctly", () => {
    expect(Number.isFinite(Infinity)).toBe(false);
    {
      let Infinity = 1;
      expect(Infinity).toBe(1);
      {
        let Infinity = 2;
        expect(Infinity).toBe(2);
      }
      expect(Infinity).toBe(1);
    }
    expect(Number.isFinite(Infinity)).toBe(false);
  });

  test("arrow function parameter can shadow Infinity", () => {
    const fn = (Infinity) => Infinity;
    expect(fn(42)).toBe(42);
    expect(fn("test")).toBe("test");
  });

  test("Infinity as a property name works", () => {
    const obj = { Infinity: "prop-value" };
    expect(obj.Infinity).toBe("prop-value");
    expect(obj["Infinity"]).toBe("prop-value");
  });

  test("Infinity as a method name works", () => {
    const obj = {
      Infinity() {
        return "method-value";
      },
    };
    expect(obj.Infinity()).toBe("method-value");
  });
});
