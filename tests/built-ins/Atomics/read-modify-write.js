describe("Atomics read-modify-write methods", () => {
  test("and, or, xor, sub, and exchange operate atomically", () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);

    i32[0] = 0b1100;
    expect(Atomics.and(i32, 0, 0b1010)).toBe(0b1100);
    expect(i32[0]).toBe(0b1000);

    expect(Atomics.or(i32, 0, 0b0011)).toBe(0b1000);
    expect(i32[0]).toBe(0b1011);

    expect(Atomics.xor(i32, 0, 0b1111)).toBe(0b1011);
    expect(i32[0]).toBe(0b0100);

    expect(Atomics.sub(i32, 0, 2)).toBe(4);
    expect(i32[0]).toBe(2);

    expect(Atomics.exchange(i32, 0, 99)).toBe(2);
    expect(i32[0]).toBe(99);
  });
});
