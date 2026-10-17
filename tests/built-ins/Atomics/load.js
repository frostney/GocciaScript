describe("Atomics.load", () => {
  test("reads integer data and preserves the element kind", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = -41;
    expect(Atomics.load(i32, 0)).toBe(-41);
  });

  test("validates the target and index", () => {
    expect(() => Atomics.load(new Float32Array(new SharedArrayBuffer(4)), 0)).toThrow(TypeError);
    expect(() => Atomics.load(new Int32Array(new SharedArrayBuffer(4)), 1)).toThrow(RangeError);
  });

  test("throws RangeError when the index's valueOf shrinks a length-tracking view past the element", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab);
    const index = {
      valueOf() {
        rab.resize(4);
        u8.length;
        return 60;
      },
    };

    expect(() => Atomics.load(u8, index)).toThrow(RangeError);
  });

  test("throws RangeError when the index's valueOf shrinks a BigInt64Array's buffer past the element", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const big = new BigInt64Array(rab);
    const index = {
      valueOf() {
        rab.resize(8);
        big.length;
        return 7;
      },
    };

    expect(() => Atomics.load(big, index)).toThrow(RangeError);
  });

  test("throws TypeError when the index's valueOf shrinks the buffer under a fixed-length view", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const i32 = new Int32Array(rab, 0, 16);
    const index = {
      valueOf() {
        rab.resize(4);
        return 15;
      },
    };

    expect(() => Atomics.load(i32, index)).toThrow(TypeError);
  });

  test("throws TypeError when the index's valueOf detaches the buffer", () => {
    const rab = new ArrayBuffer(16, { maxByteLength: 16 });
    const i32 = new Int32Array(rab);
    const index = {
      valueOf() {
        rab.transfer();
        return 0;
      },
    };

    expect(() => Atomics.load(i32, index)).toThrow(TypeError);
  });

  test("reads an element of a length-tracking view that is still inside the shrunk buffer", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab);
    u8.fill(9);
    rab.resize(4);

    expect(Atomics.load(u8, 3)).toBe(9);
    expect(() => Atomics.load(u8, 63)).toThrow(RangeError);
  });

  test("reads zeros past the old end of a length-tracking view after its buffer grew", () => {
    const rab = new ArrayBuffer(4, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab);
    u8.fill(7);
    rab.resize(64);

    expect(Atomics.load(u8, 3)).toBe(7);
    expect(Atomics.load(u8, 63)).toBe(0);
  });

  test("throws TypeError for a view past the end of its shrunk buffer", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab, 0, 32);
    rab.resize(4);

    expect(() => Atomics.load(u8, 0)).toThrow(TypeError);
  });

  test("throws TypeError for a view of a detached buffer", () => {
    const rab = new ArrayBuffer(8, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab);
    rab.transfer();

    expect(() => Atomics.load(u8, 0)).toThrow(TypeError);
  });

  test("reads the current block of a SharedArrayBuffer after it grew", () => {
    const sab = new SharedArrayBuffer(4, { maxByteLength: 64 });
    const u8 = new Uint8Array(sab);
    u8.fill(3);
    sab.grow(64);

    expect(Atomics.load(u8, 3)).toBe(3);
    expect(Atomics.load(u8, 63)).toBe(0);
  });
});
