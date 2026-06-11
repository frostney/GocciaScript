/*---
description: Computed property access on string primitive receivers
features: [property-access, String.prototype]
---*/

describe("computed property access on string primitives", () => {
  test("computed 'length' key resolves on a string primitive", () => {
    const s = "abc";
    const k = "length";
    expect(s[k]).toBe(3);
  });

  test("computed key resolves String.prototype methods", () => {
    const s = "abc";
    const k = "toUpperCase";
    expect(s[k]()).toBe("ABC");
  });

  test("computed integer key returns the code unit", () => {
    const s = "abc";
    const k = 1;
    expect(s[k]).toBe("b");
  });

  test("computed numeric string key returns the code unit", () => {
    const s = "abc";
    const k = "1";
    expect(s[k]).toBe("b");
  });

  test("computed out-of-range index returns undefined", () => {
    const s = "abc";
    expect(s[5]).toBeUndefined();
  });

  test("computed negative index returns undefined", () => {
    const s = "abc";
    expect(s[-1]).toBeUndefined();
  });

  test("computed unknown name key returns undefined", () => {
    const s = "abc";
    const k = "noSuchProperty";
    expect(s[k]).toBeUndefined();
  });

  test("computed well-known symbol key resolves on a string primitive", () => {
    const s = "abc";
    const k = Symbol.iterator;
    expect(typeof s[k]).toBe("function");
  });

  test("computed key resolving via a non-identifier name returns undefined", () => {
    const s = "abc";
    const k = "1.5";
    expect(s[k]).toBeUndefined();
  });

  test("computed assignment to 'length' on a string primitive throws", () => {
    const s = "abc";
    const k = "length";
    expect(() => {
      s[k] = 5;
    }).toThrow(TypeError);
  });

  test("computed assignment to an index on a string primitive throws", () => {
    const s = "abc";
    expect(() => {
      s[0] = "z";
    }).toThrow(TypeError);
  });

  test("computed assignment to a fresh name on a string primitive throws", () => {
    const s = "abc";
    const k = "foo";
    expect(() => {
      s[k] = 1;
    }).toThrow(TypeError);
  });
});
