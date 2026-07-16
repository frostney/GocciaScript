describe("Atomics.and", () => {
  test("returns the old value and stores the bitwise conjunction", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = 0b1100;
    expect(Atomics.and(i32, 0, 0b1010)).toBe(0b1100);
    expect(i32[0]).toBe(0b1000);
  });

  test("rejects non-integer typed arrays", () => {
    expect(() => Atomics.and(new Float32Array(new SharedArrayBuffer(4)), 0, 1)).toThrow(TypeError);
  });
});
