/*---
description: Non-strict compatibility binds regular-function nullish this to globalThis
features: [compat-function, compat-non-strict-mode]
---*/

describe("non-strict function this binding", () => {
  test("unattached function calls bind this to globalThis", () => {
    function f() {
      return this;
    }

    expect(f()).toBe(globalThis);
  });

  test("call and apply coerce nullish this values to globalThis", () => {
    function f() {
      return this;
    }

    expect(f.call(undefined)).toBe(globalThis);
    expect(f.call(null)).toBe(globalThis);
    expect(f.apply(undefined, [])).toBe(globalThis);
    expect(f.apply(null, [])).toBe(globalThis);
  });

  test("arrow functions keep lexical this", () => {
    function outer() {
      const arrow = () => this;
      return arrow();
    }

    const receiver = { marker: 1 };
    expect(outer.call(receiver)).toBe(receiver);
  });
});
