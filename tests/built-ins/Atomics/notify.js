describe("Atomics.notify", () => {
  test("returns zero when there are no waiters", () => {
    expect(Atomics.notify(new Int32Array(new SharedArrayBuffer(4)), 0)).toBe(0);
  });

  test("returns zero for non-shared waitable arrays", () => {
    expect(Atomics.notify(new Int32Array(1), 0)).toBe(0);
    expect(Atomics.notify(new BigInt64Array(1), 0)).toBe(0);
  });

  test("rejects invalid waitable typed arrays", () => {
    expect(() => Atomics.notify(new Uint32Array(new SharedArrayBuffer(4)), 0)).toThrow(TypeError);
  });

  test("throws TypeError for a view past the end of its shrunk buffer", () => {
    const rab = new ArrayBuffer(16, { maxByteLength: 16 });
    const i32 = new Int32Array(rab, 0, 4);
    rab.resize(8);

    expect(() => Atomics.notify(i32, 0)).toThrow(TypeError);
  });

  test("measures the length before converting the index", () => {
    const rab = new ArrayBuffer(4, { maxByteLength: 4 });
    const i32 = new Int32Array(rab);
    const index = {
      valueOf() {
        rab.resize(0);
        return 0;
      },
    };

    expect(Atomics.notify(i32, index)).toBe(0);
  });
});
