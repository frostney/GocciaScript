describe("TypedArray integer-indexed defineProperty", () => {
  test("defines a numeric value descriptor with omitted attributes", () => {
    const ta = new Uint8Array([1]);

    expect(Reflect.defineProperty(ta, "0", { value: 7 })).toBe(true);

    expect(ta[0]).toBe(7);
    expect(Object.getOwnPropertyDescriptor(ta, "0")).toEqual({
      value: 7,
      writable: true,
      enumerable: true,
      configurable: true,
    });
  });

  test("rejects incompatible numeric descriptors", () => {
    const cases = [
      { value: 1, configurable: false },
      { value: 1, enumerable: false },
      { value: 1, writable: false },
      { get: () => 1 },
      { set: () => {} },
    ];

    for (const descriptor of cases) {
      const ta = new Uint8Array([0]);

      expect(Reflect.defineProperty(ta, "0", descriptor)).toBe(false);
      expect(() => Object.defineProperty(ta, "0", descriptor)).toThrow(
        TypeError
      );
      expect(ta[0]).toBe(0);
    }
  });

  test("rejects invalid integer indexes without converting the value", () => {
    const ta = new Uint8Array([0]);
    let valueOfCalls = 0;
    const value = {
      valueOf() {
        valueOfCalls += 1;
        return 9;
      },
    };

    expect(Reflect.defineProperty(ta, "1", { value })).toBe(false);
    expect(Reflect.defineProperty(ta, "-1", { value })).toBe(false);
    expect(Reflect.defineProperty(ta, "1.5", { value })).toBe(false);
    expect(Reflect.defineProperty(ta, "-0", { value })).toBe(false);
    expect(() => Object.defineProperty(ta, "1", { value })).toThrow(TypeError);

    expect(valueOfCalls).toBe(0);
    expect(ta[0]).toBe(0);
  });

  test("converts values for valid indexes and propagates conversion errors", () => {
    const ta = new Uint8Array([0]);
    let valueOfCalls = 0;
    const value = {
      valueOf() {
        valueOfCalls += 1;
        return 260;
      },
    };

    expect(Reflect.defineProperty(ta, "0", { value })).toBe(true);
    expect(valueOfCalls).toBe(1);
    expect(ta[0]).toBe(4);

    expect(() =>
      Reflect.defineProperty(new Uint8Array([0]), "0", {
        value: {
          valueOf() {
            throw new Error("conversion failed");
          },
        },
      })
    ).toThrow(Error);
  });

  test("uses BigInt conversion rules for BigInt typed arrays", () => {
    const ta = new BigInt64Array([0n]);

    expect(Reflect.defineProperty(ta, "0", { value: 3n })).toBe(true);
    expect(ta[0]).toBe(3n);
    expect(() =>
      Reflect.defineProperty(new BigInt64Array([0n]), "0", { value: 1 })
    ).toThrow(TypeError);
  });

  test("rejects numeric indexes on detached buffers", () => {
    const buffer = new ArrayBuffer(1);
    const ta = new Uint8Array(buffer);

    buffer.transfer();

    expect(Reflect.defineProperty(ta, "0", { value: 1 })).toBe(false);
    expect(Reflect.defineProperty(ta, "1", { value: 1 })).toBe(false);
    expect(() => Object.defineProperty(ta, "0", { value: 1 })).toThrow(
      TypeError
    );
  });
});
