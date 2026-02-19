/*---
description: >
  undefined is a global property (not a reserved keyword) per ECMAScript spec.
  It can be shadowed by local variable declarations.
features: [global-properties, undefined, shadowing]
---*/

describe("undefined as a global property", () => {
  test("global undefined has the correct value", () => {
    expect(undefined).toBeUndefined();
  });

  test("typeof undefined returns 'undefined'", () => {
    expect(typeof undefined).toBe("undefined");
  });

  test("undefined is strictly equal to itself", () => {
    expect(undefined === undefined).toBe(true);
  });

  test("undefined is not equal to null with strict equality", () => {
    expect(undefined === null).toBe(false);
  });

  test("undefined is falsy", () => {
    let result = "truthy";
    if (!undefined) {
      result = "falsy";
    }
    expect(result).toBe("falsy");
  });

  test("undefined can be used in arithmetic (produces NaN)", () => {
    expect(Number.isNaN(undefined + 1)).toBe(true);
    expect(Number.isNaN(undefined * 2)).toBe(true);
  });

  test("undefined converts to 'undefined' string", () => {
    expect(`${undefined}`).toBe("undefined");
    expect("" + undefined).toBe("undefined");
  });
});

describe("undefined immutability", () => {
  test("global undefined cannot be reassigned", () => {
    expect(() => {
      undefined = 42;
    }).toThrow();
  });

  test("global undefined remains unchanged after failed reassignment", () => {
    try {
      undefined = "overwritten";
    } catch (e) {
      // expected
    }
    expect(undefined).toBeUndefined();
  });
});

describe("undefined shadowing", () => {
  test("undefined can be shadowed in a block scope with let", () => {
    expect(undefined).toBeUndefined();
    {
      let undefined = 42;
      expect(undefined).toBe(42);
    }
    expect(undefined).toBeUndefined();
  });

  test("undefined can be shadowed in a block scope with const", () => {
    {
      const undefined = "shadowed";
      expect(undefined).toBe("shadowed");
    }
    expect(undefined).toBeUndefined();
  });

  test("shadowed undefined does not affect outer scope", () => {
    const outer = undefined;
    {
      let undefined = 100;
      expect(undefined).toBe(100);
    }
    expect(outer).toBeUndefined();
  });

  test("shadowed undefined can be any type", () => {
    {
      let undefined = 42;
      expect(typeof undefined).toBe("number");
    }
    {
      let undefined = "hello";
      expect(typeof undefined).toBe("string");
    }
    {
      let undefined = true;
      expect(typeof undefined).toBe("boolean");
    }
    {
      let undefined = { key: "value" };
      expect(typeof undefined).toBe("object");
    }
    {
      let undefined = (x) => x;
      expect(typeof undefined).toBe("function");
    }
  });

  test("nested shadowing restores correctly", () => {
    expect(undefined).toBeUndefined();
    {
      let undefined = 1;
      expect(undefined).toBe(1);
      {
        let undefined = 2;
        expect(undefined).toBe(2);
        {
          let undefined = 3;
          expect(undefined).toBe(3);
        }
        expect(undefined).toBe(2);
      }
      expect(undefined).toBe(1);
    }
    expect(undefined).toBeUndefined();
  });

  test("arrow function parameter can shadow undefined", () => {
    const fn = (undefined) => undefined;
    expect(fn(42)).toBe(42);
    expect(fn("test")).toBe("test");
  });

  test("undefined as a property name works", () => {
    const obj = { undefined: "prop-value" };
    expect(obj.undefined).toBe("prop-value");
    expect(obj["undefined"]).toBe("prop-value");
  });

  test("undefined as a method name works", () => {
    const obj = {
      undefined() {
        return "method-value";
      },
    };
    expect(obj.undefined()).toBe("method-value");
  });
});
