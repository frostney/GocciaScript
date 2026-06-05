describe("Atomics.pause", () => {
  test("returns undefined for omitted and integral Number arguments", () => {
    expect(Atomics.pause()).toBe(undefined);
    expect(Atomics.pause(undefined)).toBe(undefined);
    expect(Atomics.pause(0)).toBe(undefined);
    expect(Atomics.pause(-0)).toBe(undefined);
    expect(Atomics.pause(42)).toBe(undefined);
    expect(Atomics.pause(Number.MAX_SAFE_INTEGER)).toBe(undefined);
  });

  test("throws TypeError for non-integral or non-Number arguments", () => {
    expect(() => Atomics.pause(42.42)).toThrow(TypeError);
    expect(() => Atomics.pause(NaN)).toThrow(TypeError);
    expect(() => Atomics.pause(Infinity)).toThrow(TypeError);
    expect(() => Atomics.pause("42")).toThrow(TypeError);
    expect(() => Atomics.pause(42n)).toThrow(TypeError);
    expect(() => Atomics.pause({ valueOf() { return 42; } })).toThrow(
      TypeError,
    );
  });
});
