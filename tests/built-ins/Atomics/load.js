describe("Atomics.load", () => {
  test("reads integer data and preserves the element kind", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = -41;
    expect(Atomics.load(i32, 0)).toBe(-41);
  });

  test("validates the target and index", () => {
    expect(() => Atomics.load(new Float32Array(new SharedArrayBuffer(4)), 0)).toThrow(TypeError);
    expect(() => Atomics.load(new Int32Array(new SharedArrayBuffer(4)), 1)).toThrow(RangeError);
  });
});
