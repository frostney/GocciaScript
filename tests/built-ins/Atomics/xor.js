describe("Atomics.xor", () => {
  test("returns the old value and stores the bitwise exclusive disjunction", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = 0b1011;
    expect(Atomics.xor(i32, 0, 0b1111)).toBe(0b1011);
    expect(i32[0]).toBe(0b0100);
  });

  test("wraps according to the target element kind", () => {
    const i8 = new Int8Array(new SharedArrayBuffer(1));
    i8[0] = -1;
    expect(Atomics.xor(i8, 0, 0xff)).toBe(-1);
    expect(i8[0]).toBe(0);
  });
});
