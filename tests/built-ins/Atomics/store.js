describe("Atomics.store", () => {
  test("writes integer data and returns the pre-wrapping value", () => {
    const i8 = new Int8Array(new SharedArrayBuffer(1));

    expect(Atomics.store(i8, 0, 12345)).toBe(12345);
    expect(i8[0]).toBe(57);
  });

  test("coerces the value once", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    let calls = 0;
    const value = {
      valueOf() {
        calls++;
        return 9;
      },
    };

    expect(Atomics.store(i32, 0, value)).toBe(9);
    expect(i32[0]).toBe(9);
    expect(calls).toBe(1);
  });
});
