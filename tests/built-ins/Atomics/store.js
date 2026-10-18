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

  test("throws RangeError and writes nothing when the value's valueOf shrinks a length-tracking view past the element", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab);
    const value = {
      valueOf() {
        rab.resize(4);
        u8.length;
        return 1;
      },
    };

    expect(() => Atomics.store(u8, 60, value)).toThrow(RangeError);
    expect(Array.from(u8)).toEqual([0, 0, 0, 0]);
  });

  test("throws RangeError when the value's valueOf shrinks the buffer so the element only partly fits", () => {
    const rab = new ArrayBuffer(16, { maxByteLength: 16 });
    const i32 = new Int32Array(rab);
    const value = {
      valueOf() {
        rab.resize(6);
        return 1;
      },
    };

    expect(() => Atomics.store(i32, 1, value)).toThrow(RangeError);
  });

  test("throws RangeError when the value's valueOf shrinks a BigInt64Array's buffer past the element", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const big = new BigInt64Array(rab);
    const value = {
      valueOf() {
        rab.resize(8);
        big.length;
        return 1n;
      },
    };

    expect(() => Atomics.store(big, 7, value)).toThrow(RangeError);
  });

  test("throws TypeError when the value's valueOf shrinks the buffer under a fixed-length view", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const i32 = new Int32Array(rab, 0, 16);
    const value = {
      valueOf() {
        rab.resize(4);
        i32.length;
        return 1;
      },
    };

    expect(() => Atomics.store(i32, 15, value)).toThrow(TypeError);
  });

  test("throws TypeError when the value's valueOf detaches the buffer", () => {
    const rab = new ArrayBuffer(16, { maxByteLength: 16 });
    const i32 = new Int32Array(rab);
    const value = {
      valueOf() {
        rab.transfer();
        return 1;
      },
    };

    expect(() => Atomics.store(i32, 0, value)).toThrow(TypeError);
  });

  test("stores into an element still inside the buffer after the value's valueOf shrank it", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab);
    const value = {
      valueOf() {
        rab.resize(4);
        return 1;
      },
    };

    expect(Atomics.store(u8, 2, value)).toBe(1);
    expect(Array.from(u8)).toEqual([0, 0, 1, 0]);
  });

  test("stores into the buffer's current block after the index's valueOf grew it", () => {
    const rab = new ArrayBuffer(4, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab);
    const index = {
      valueOf() {
        rab.resize(64);
        return 2;
      },
    };

    expect(Atomics.store(u8, index, 5)).toBe(5);
    expect(u8.length).toBe(64);
    expect(u8[2]).toBe(5);
  });

  test("stores into a fixed-length view whose buffer the value's valueOf resized to its own length", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const i32 = new Int32Array(rab, 0, 16);
    const value = {
      valueOf() {
        rab.resize(64);
        return 9;
      },
    };

    expect(Atomics.store(i32, 15, value)).toBe(9);
    expect(Atomics.load(i32, 15)).toBe(9);
  });

  test("throws TypeError for a view past the end of its shrunk buffer", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab, 0, 32);
    rab.resize(4);

    expect(() => Atomics.store(u8, 0, 1)).toThrow(TypeError);
  });
});
