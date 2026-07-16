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
});
