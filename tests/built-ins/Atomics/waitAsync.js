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

  test("notify resolves an asynchronous waiter promise", async () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);
    i32[0] = 1;

    const result = Atomics.waitAsync(i32, 0, 1);
    expect(result.async).toBe(true);
    expect(Atomics.notify(i32, 0, 1)).toBe(1);
    expect(await result.value).toBe("ok");
  });

  test("finite timeout promise resolves to timed-out", async () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);
    i32[0] = 1;

    const result = Atomics.waitAsync(i32, 0, 1, 1);
    expect(result.async).toBe(true);
    expect(await result.value).toBe("timed-out");
  });

  test("a timeout past 32 bits of milliseconds still waits", async () => {
    const sab = new SharedArrayBuffer(4);
    const i32 = new Int32Array(sab);

    const order = [];
    const long = Atomics.waitAsync(i32, 0, 0, 2 ** 32);
    const short = Atomics.waitAsync(i32, 0, 0, 1);
    expect(long.async).toBe(true);
    expect(short.async).toBe(true);
    const longDone = long.value.then((r) => order.push(`long ${r}`));
    await short.value.then((r) => {
      order.push(`short ${r}`);
      Atomics.notify(i32, 0);
    });
    await longDone;
    expect(order).toEqual(["short timed-out", "long ok"]);
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

    const result = Atomics.waitAsync(i32, 0, value, 0);
    expect(result.async).toBe(false);
    expect(result.value).toBe("timed-out");
  });
});
