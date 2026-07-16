describe("Atomics.compareExchange", () => {
  test("writes the replacement only when the expected value matches", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = 10;

    expect(Atomics.compareExchange(i32, 0, 5, 20)).toBe(10);
    expect(i32[0]).toBe(10);
    expect(Atomics.compareExchange(i32, 0, 10, 20)).toBe(10);
    expect(i32[0]).toBe(20);
  });

  test("validates the target and index", () => {
    expect(() => Atomics.compareExchange(new Float32Array(new SharedArrayBuffer(4)), 0, 0, 1)).toThrow(TypeError);
    expect(() => Atomics.compareExchange(new Int32Array(new SharedArrayBuffer(4)), 1, 0, 1)).toThrow(RangeError);
  });
});
