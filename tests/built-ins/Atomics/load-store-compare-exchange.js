describe("Atomics load, store, and compareExchange", () => {
  test("load and store read and write shared integer data", () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);

    expect(Atomics.store(i32, 0, 41)).toBe(41);
    expect(Atomics.load(i32, 0)).toBe(41);
    expect(i32[0]).toBe(41);
  });

  test("store returns ToIntegerOrInfinity before typed-array storage wrapping", () => {
    const sab = new SharedArrayBuffer(1);
    const i8 = new Int8Array(sab);

    expect(Atomics.store(i8, 0, 12345)).toBe(12345);
    expect(i8[0]).toBe(57);
  });

  test("compareExchange writes replacement only when expected matches", () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);
    i32[0] = 10;

    expect(Atomics.compareExchange(i32, 0, 5, 20)).toBe(10);
    expect(i32[0]).toBe(10);

    expect(Atomics.compareExchange(i32, 0, 10, 20)).toBe(10);
    expect(i32[0]).toBe(20);
  });
});
