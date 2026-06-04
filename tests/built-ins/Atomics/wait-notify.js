describe("Atomics.wait and Atomics.notify", () => {
  test("wait returns not-equal when current value differs", () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);
    i32[0] = 1;

    expect(Atomics.wait(i32, 0, 2, 10)).toBe("not-equal");
  });

  test("wait with zero timeout returns timed-out", () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);
    i32[0] = 1;

    expect(Atomics.wait(i32, 0, 1, 0)).toBe("timed-out");
  });

  test("notify returns zero when there are no waiters", () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);

    expect(Atomics.notify(i32, 0)).toBe(0);
  });

  test("notify returns zero for non-shared waitable arrays", () => {
    const i32 = new Int32Array(1);
    const i64 = new BigInt64Array(1);

    expect(Atomics.notify(i32, 0)).toBe(0);
    expect(Atomics.notify(i64, 0)).toBe(0);
  });

  test("wait only accepts Int32Array and BigInt64Array", () => {
    const sab = new SharedArrayBuffer(4);
    const u32 = new Uint32Array(sab);

    expect(() => Atomics.wait(u32, 0, 0, 0)).toThrow(TypeError);
  });
});
