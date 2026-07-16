describe("Atomics.or", () => {
  test("returns the old value and stores the bitwise disjunction", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = 0b1000;
    expect(Atomics.or(i32, 0, 0b0011)).toBe(0b1000);
    expect(i32[0]).toBe(0b1011);
  });

  test("rejects out-of-bounds indices", () => {
    expect(() => Atomics.or(new Int32Array(new SharedArrayBuffer(4)), 1, 1)).toThrow(RangeError);
  });
});
