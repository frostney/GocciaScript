describe("Atomics with BigInt typed arrays", () => {
  test("load, store, add, and compareExchange use BigInt values", () => {
    const sab = new SharedArrayBuffer(8);
    const big = new BigInt64Array(sab);

    expect(Atomics.store(big, 0, 5n)).toBe(5n);
    expect(Atomics.load(big, 0)).toBe(5n);
    expect(Atomics.add(big, 0, 3n)).toBe(5n);
    expect(big[0]).toBe(8n);
    expect(Atomics.compareExchange(big, 0, 8n, -1n)).toBe(8n);
    expect(big[0]).toBe(-1n);
  });

  test("throws TypeError when BigInt atomics receive Number values", () => {
    const sab = new SharedArrayBuffer(8);
    const big = new BigInt64Array(sab);

    expect(() => Atomics.store(big, 0, 1)).toThrow(TypeError);
  });

  test("store returns BigInt(value) before typed-array storage wrapping", () => {
    const sab = new SharedArrayBuffer(8);
    const big = new BigInt64Array(sab);
    const value = { valueOf() { return 33n; } };

    expect(Atomics.store(big, 0, value)).toBe(33n);
    expect(big[0]).toBe(33n);
    expect(BigInt(value)).toBe(33n);
  });
});
