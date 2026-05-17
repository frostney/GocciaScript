describe("TypedArray integer-indexed [[Set]] in non-strict compatibility mode", () => {
  test("numeric element assignment writes the backing storage", () => {
    const ta = new Uint8Array(2);
    ta[0] = 17;
    ta[1] = 23;

    expect(ta[0]).toBe(17);
    expect(ta[1]).toBe(23);
  });

  test("string numeric element assignment writes the backing storage", () => {
    const ta = new Uint8Array(1);
    ta["0"] = 91;

    expect(ta[0]).toBe(91);
  });

  test("out-of-bounds numeric element assignment still coerces the value", () => {
    const ta = new Uint8Array(1);

    expect(() => {
      ta[1] = Symbol.iterator;
    }).toThrow(TypeError);
    expect(ta[0]).toBe(0);
  });

  test("non-canonical numeric-looking keys stay ordinary properties", () => {
    const ta = new Uint8Array(2);
    ta["01"] = 17;

    expect(ta[1]).toBe(0);
    expect(ta["01"]).toBe(17);
  });
});
