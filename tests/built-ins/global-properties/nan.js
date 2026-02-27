/*---
description: >
  NaN is a global property (not a keyword) per ECMAScript spec.
  It can be shadowed by local variable declarations.
features: [global-properties, NaN, shadowing]
---*/

describe("NaN as a global property", () => {
  test("global NaN has the correct value", () => {
    expect(Number.isNaN(NaN)).toBe(true);
  });

  test("typeof NaN returns 'number'", () => {
    expect(typeof NaN).toBe("number");
  });

  test("NaN is not equal to itself", () => {
    expect(NaN === NaN).toBe(false);
    expect(NaN !== NaN).toBe(true);
  });

  test("NaN is falsy", () => {
    let result = "truthy";
    if (!NaN) {
      result = "falsy";
    }
    expect(result).toBe("falsy");
  });

  test("NaN propagates through arithmetic", () => {
    expect(Number.isNaN(NaN + 1)).toBe(true);
    expect(Number.isNaN(NaN * 2)).toBe(true);
    expect(Number.isNaN(NaN - 0)).toBe(true);
    expect(Number.isNaN(NaN / 10)).toBe(true);
  });

  test("NaN converts to 'NaN' string", () => {
    expect(`${NaN}`).toBe("NaN");
    expect("" + NaN).toBe("NaN");
  });
});

describe("NaN immutability", () => {
  test("global NaN cannot be reassigned", () => {
    expect(() => {
      NaN = 42;
    }).toThrow(TypeError);
  });

  test("global NaN remains unchanged after failed reassignment", () => {
    try {
      NaN = 0;
    } catch (e) {
      // expected
    }
    expect(Number.isNaN(NaN)).toBe(true);
  });
});

describe("NaN shadowing", () => {
  test("NaN can be shadowed in a block scope with let", () => {
    expect(Number.isNaN(NaN)).toBe(true);
    {
      let NaN = 42;
      expect(NaN).toBe(42);
    }
    expect(Number.isNaN(NaN)).toBe(true);
  });

  test("NaN can be shadowed in a block scope with const", () => {
    {
      const NaN = "not a number";
      expect(NaN).toBe("not a number");
    }
    expect(Number.isNaN(NaN)).toBe(true);
  });

  test("shadowed NaN does not affect outer scope", () => {
    const outer = NaN;
    {
      let NaN = 100;
      expect(NaN).toBe(100);
    }
    expect(Number.isNaN(outer)).toBe(true);
  });

  test("nested shadowing restores correctly", () => {
    expect(Number.isNaN(NaN)).toBe(true);
    {
      let NaN = 1;
      expect(NaN).toBe(1);
      {
        let NaN = 2;
        expect(NaN).toBe(2);
      }
      expect(NaN).toBe(1);
    }
    expect(Number.isNaN(NaN)).toBe(true);
  });

  test("arrow function parameter can shadow NaN", () => {
    const fn = (NaN) => NaN;
    expect(fn(42)).toBe(42);
    expect(fn("test")).toBe("test");
  });

  test("NaN as a property name works", () => {
    const obj = { NaN: "prop-value" };
    expect(obj.NaN).toBe("prop-value");
    expect(obj["NaN"]).toBe("prop-value");
  });

  test("NaN as a method name works", () => {
    const obj = {
      NaN() {
        return "method-value";
      },
    };
    expect(obj.NaN()).toBe("method-value");
  });
});
