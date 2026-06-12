/*---
description: Computed property access on number, boolean, bigint, and symbol primitive receivers
features: [property-access, Number.prototype, Boolean.prototype, BigInt.prototype, Symbol.prototype]
---*/

describe("computed property access on non-string primitives", () => {
  test("computed key resolves Number.prototype methods", () => {
    const n = 5;
    const k = "toFixed";
    expect(n[k](2)).toBe("5.00");
  });

  test("computed key resolves Boolean.prototype methods", () => {
    const b = true;
    const k = "toString";
    expect(b[k]()).toBe("true");
  });

  test("computed key resolves BigInt.prototype methods", () => {
    const x = 255n;
    const k = "toString";
    expect(x[k](16)).toBe("ff");
  });

  test("computed key resolves Symbol.prototype accessors", () => {
    const s = Symbol("hello");
    const k = "description";
    expect(s[k]).toBe("hello");
  });

  test("computed well-known symbol key resolves on a symbol primitive", () => {
    const s = Symbol("hello");
    const k = Symbol.toPrimitive;
    expect(typeof s[k]).toBe("function");
  });

  test("computed unknown name key on a number returns undefined", () => {
    const n = 5;
    const k = "noSuchProperty";
    expect(n[k]).toBeUndefined();
  });

  test("computed key colliding with internal private-slot naming returns undefined on a number", () => {
    const n = 5;
    const k = "#slot:x";
    expect(n[k]).toBeUndefined();
  });

  test("computed well-known symbol key missing from Number.prototype returns undefined", () => {
    const n = 5;
    const k = Symbol.iterator;
    expect(n[k]).toBeUndefined();
  });
});
