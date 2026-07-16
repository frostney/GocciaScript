describe("Atomics.exchange", () => {
  test("returns the old value and stores the replacement", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = 2;
    expect(Atomics.exchange(i32, 0, 99)).toBe(2);
    expect(i32[0]).toBe(99);
  });

  test("coerces the replacement before storing it", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    expect(Atomics.exchange(i32, 0, "12")).toBe(0);
    expect(i32[0]).toBe(12);
  });
});
