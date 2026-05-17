"use strict";

describe("strict directive arguments object", () => {
  test("strict script functions still receive unmapped arguments objects", () => {
    function capture() {
      return arguments;
    }

    const args = capture(1, 2);

    expect(args.length).toBe(2);
    expect(args[0]).toBe(1);
    expect(() => {
      args.callee = {};
    }).toThrow(TypeError);
  });
});
