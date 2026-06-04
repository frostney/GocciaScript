describe("Atomics.waitAsync", () => {
  test("returns synchronous not-equal result when current value differs", () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);
    i32[0] = 1;

    const result = Atomics.waitAsync(i32, 0, 2, 10);
    expect(result.async).toBe(false);
    expect(result.value).toBe("not-equal");
  });

  test("returns synchronous timed-out result for zero timeout", () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);
    i32[0] = 1;

    const result = Atomics.waitAsync(i32, 0, 1, 0);
    expect(result.async).toBe(false);
    expect(result.value).toBe("timed-out");
  });

  test("returns a pending promise when the wait is asynchronous", () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);
    i32[0] = 1;

    const result = Atomics.waitAsync(i32, 0, 1);
    expect(result.async).toBe(true);
    expect(result.value instanceof Promise).toBe(true);
    expect(Atomics.notify(i32, 0, 1)).toBe(1);
  });

  test("finite timeout promise resolves to timed-out", async () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);
    i32[0] = 1;

    const result = Atomics.waitAsync(i32, 0, 1, 1);
    expect(result.async).toBe(true);
    expect(await result.value).toBe("timed-out");
  });
});
