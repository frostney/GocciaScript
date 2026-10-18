describe("Atomics.wait", () => {
  test("returns not-equal when the current value differs", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = 1;
    expect(Atomics.wait(i32, 0, 2, 10)).toBe("not-equal");
  });

  test("returns timed-out for a matching value and zero timeout", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = 1;
    expect(Atomics.wait(i32, 0, 1, 0)).toBe("timed-out");
  });

  test("accepts only Int32Array and BigInt64Array views", () => {
    expect(() => Atomics.wait(new Uint32Array(new SharedArrayBuffer(4)), 0, 0, 0)).toThrow(TypeError);
  });

  test("compares against the current block after the value's valueOf grew the buffer", () => {
    const sab = new SharedArrayBuffer(4, { maxByteLength: 64 });
    const i32 = new Int32Array(sab);
    const value = {
      valueOf() {
        sab.grow(64);
        new Int32Array(sab)[0] = 1;
        return 1;
      },
    };

    expect(Atomics.wait(i32, 0, value, 0)).toBe("timed-out");
  });
});
