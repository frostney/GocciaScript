/*---
description: Strict-by-default assignment semantics without non-strict compatibility
---*/

describe("default assignment semantics", () => {
  test("read-only property writes throw", () => {
    const obj = {};
    Object.defineProperty(obj, "fixed", {
      value: 1,
      writable: false,
      configurable: true,
    });

    expect(() => {
      obj.fixed = 2;
    }).toThrow(TypeError);
  });
});
