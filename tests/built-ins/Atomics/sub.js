describe("Atomics.sub", () => {
  test("returns the old value and stores the difference", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = 4;
    expect(Atomics.sub(i32, 0, 2)).toBe(4);
    expect(i32[0]).toBe(2);
  });

  test("wraps integer overflow according to the target element kind", () => {
    const i8 = new Int8Array(new SharedArrayBuffer(1));
    i8[0] = -128;
    expect(Atomics.sub(i8, 0, 1)).toBe(-128);
    expect(i8[0]).toBe(127);
  });
});
