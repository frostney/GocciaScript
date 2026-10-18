describe("Atomics.or", () => {
  test("returns the old value and stores the bitwise disjunction", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = 0b1000;
    expect(Atomics.or(i32, 0, 0b0011)).toBe(0b1000);
    expect(i32[0]).toBe(0b1011);
  });

  test("rejects out-of-bounds indices", () => {
    expect(() => Atomics.or(new Int32Array(new SharedArrayBuffer(4)), 1, 1)).toThrow(RangeError);
  });

  test("converts the operand before it reads the element", () => {
    const i32 = new Int32Array(4);
    i32[0] = 1;
    const operand = {
      valueOf() {
        i32[0] = 12;
        return 1;
      },
    };

    expect(Atomics.or(i32, 0, operand)).toBe(12);
    expect(i32[0]).toBe(13);
  });

  test("throws RangeError and writes nothing when the operand shrinks a length-tracking view past the element", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab);
    const operand = {
      valueOf() {
        rab.resize(4);
        u8.length;
        return 1;
      },
    };

    expect(() => Atomics.or(u8, 60, operand)).toThrow(RangeError);
    expect(Array.from(u8)).toEqual([0, 0, 0, 0]);
  });

  test("throws RangeError when the operand shrinks the buffer so the element only partly fits", () => {
    const rab = new ArrayBuffer(16, { maxByteLength: 16 });
    const i32 = new Int32Array(rab);
    const operand = {
      valueOf() {
        rab.resize(6);
        return 1;
      },
    };

    expect(() => Atomics.or(i32, 1, operand)).toThrow(RangeError);
    expect(rab.byteLength).toBe(6);
  });

  test("throws RangeError when the operand shrinks a BigInt64Array's buffer past the element", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const big = new BigInt64Array(rab);
    const operand = {
      valueOf() {
        rab.resize(8);
        big.length;
        return 1n;
      },
    };

    expect(() => Atomics.or(big, 7, operand)).toThrow(RangeError);
  });

  test("throws TypeError when the operand shrinks the buffer under a fixed-length view", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const i32 = new Int32Array(rab, 0, 16);
    const operand = {
      valueOf() {
        rab.resize(4);
        i32.length;
        return 1;
      },
    };

    expect(() => Atomics.or(i32, 15, operand)).toThrow(TypeError);
  });

  test("throws TypeError when the operand detaches the buffer", () => {
    const rab = new ArrayBuffer(16, { maxByteLength: 16 });
    const i32 = new Int32Array(rab);
    const operand = {
      valueOf() {
        rab.transfer();
        return 1;
      },
    };

    expect(() => Atomics.or(i32, 0, operand)).toThrow(TypeError);
  });

  test("writes the element in the buffer's current block after the operand grew the buffer", () => {
    const rab = new ArrayBuffer(4, { maxByteLength: 64 });
    const i32 = new Int32Array(rab);
    i32[0] = 1;
    const operand = {
      valueOf() {
        rab.resize(64);
        return 1;
      },
    };

    expect(Atomics.or(i32, 0, operand)).toBe(1);
    expect(i32.length).toBe(16);
    expect(new Int32Array(rab)[0]).toBe(1);
  });

  test("throws TypeError before converting the index for a view past the end of its shrunk buffer", () => {
    const rab = new ArrayBuffer(16, { maxByteLength: 16 });
    const i32 = new Int32Array(rab, 0, 4);
    rab.resize(8);
    let converted = false;
    const index = {
      valueOf() {
        converted = true;
        return 0;
      },
    };

    expect(() => Atomics.or(i32, index, 1)).toThrow(TypeError);
    expect(converted).toBe(false);
  });
});
