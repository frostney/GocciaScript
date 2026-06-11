describe("explicit arguments object compatibility", () => {
  test("enables arguments without non-strict this binding", () => {
    function capture(first) {
      arguments[0] = "argument";
      first = "parameter";
      return [arguments.length, arguments[0], first, this];
    }

    expect(capture("value")).toEqual([1, "argument", "parameter", undefined]);
  });

  test("does not enable non-strict assignment or delete semantics", () => {
    const obj = {};
    Object.defineProperty(obj, "fixed", {
      value: 1,
      writable: false,
      configurable: false
    });

    expect(() => {
      obj.fixed = 2;
    }).toThrow(TypeError);
    expect(() => {
      delete obj.fixed;
    }).toThrow(TypeError);
  });

  test("strict directive functions receive unmapped arguments objects", () => {
    function capture(first) {
      "use strict";
      arguments[0] = "argument";
      first = "parameter";
      return [arguments[0], first, this];
    }

    expect(capture("value")).toEqual(["argument", "parameter", undefined]);
  });
});
