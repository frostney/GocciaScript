/*---
description: >
  A typed array answers a read from its elements only for a canonical numeric
  string; every other name is an ordinary property lookup
features: [TypedArray]
---*/

describe("TypedArray named property access", () => {
  test("a name that is not a canonical numeric string is read from the prototype chain", () => {
    const names = [
      "1e3", "1e21", "01", "+1", " 1", "1 ", ".5", "0x1", "1_0", "Infinityx", "NaNa",
      "-", "--1", "-foo", "-Infinityx", "I", "N", "", "label",
    ];
    const prototype = {};
    for (const name of names) prototype[name] = "inherited " + name;
    const ta = Object.setPrototypeOf(new Uint8Array([7, 8, 9]), prototype);

    for (const name of names) {
      expect(ta[name]).toBe("inherited " + name);
      expect(name in ta).toBe(true);
      expect(Object.hasOwn(ta, name)).toBe(false);
    }
  });

  test("a canonical numeric string that is not a valid index never reaches the prototype chain", () => {
    const names = ["3", "-0", "-1", "1.5", "0.5", "Infinity", "-Infinity", "NaN", "1e+21", "1e-7"];
    const prototype = {};
    for (const name of names) prototype[name] = "inherited " + name;
    const ta = Object.setPrototypeOf(new Uint8Array([7, 8, 9]), prototype);

    for (const name of names) {
      expect(ta[name]).toBeUndefined();
      expect(name in ta).toBe(false);
      expect(Object.hasOwn(ta, name)).toBe(false);
    }
    expect(ta["0"]).toBe(7);
    expect(ta["2"]).toBe(9);
  });

  test("a property the global object creates on first use is found through a typed array", () => {
    const ta = Object.setPrototypeOf(new Uint8Array(1), globalThis);

    // The read through the typed array is the first use of Atomics in this file.
    expect(ta.Atomics).toBe(Atomics);
    expect(typeof ta.Atomics.load).toBe("function");
  });

  test("another built-in function used as a getter is called with the typed array as receiver", () => {
    const holder = {};
    Object.defineProperty(holder, "self", { get: Object.prototype.valueOf });
    Object.defineProperty(holder, "invoked", { get: Function.prototype.call });
    const ta = Object.setPrototypeOf(new Uint8Array(2), holder);

    expect(ta.self).toBe(ta);
    expect(() => ta.invoked).toThrow(TypeError);
  });

  test("a method is read from %TypedArray%.prototype and from an own property that shadows it", () => {
    const ta = new Uint8Array([7, 8, 9]);

    expect(ta.at).toBe(Object.getPrototypeOf(Uint8Array.prototype).at);
    expect(ta.at(1)).toBe(8);

    ta.at = () => "own";
    expect(ta.at(1)).toBe("own");
    expect(Object.hasOwn(ta, "at")).toBe(true);
  });
});
