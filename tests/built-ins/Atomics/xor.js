describe("Atomics.xor", () => {
  test("returns the old value and stores the bitwise exclusive disjunction", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = 0b1011;
    expect(Atomics.xor(i32, 0, 0b1111)).toBe(0b1011);
    expect(i32[0]).toBe(0b0100);
  });

  test("wraps according to the target element kind", () => {
    const i8 = new Int8Array(new SharedArrayBuffer(1));
    i8[0] = -1;
    expect(Atomics.xor(i8, 0, 0xff)).toBe(-1);
    expect(i8[0]).toBe(0);
  });

  test("converts the operand before it reads the element", () => {
    const i32 = new Int32Array(4);
    i32[0] = 1;
    const operand = {
      valueOf() {
        i32[0] = 12;
        return 6;
      },
    };

    expect(Atomics.xor(i32, 0, operand)).toBe(12);
    expect(i32[0]).toBe(10);
  });

  test("throws RangeError and writes nothing when the operand shrinks a length-tracking view past the element", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab);
    const operand = {
      valueOf() {
        rab.resize(4);
        u8.length;
        return 6;
      },
    };

    expect(() => Atomics.xor(u8, 60, operand)).toThrow(RangeError);
    expect(Array.from(u8)).toEqual([0, 0, 0, 0]);
  });

  test("throws RangeError when the operand shrinks the buffer so the element only partly fits", () => {
    const rab = new ArrayBuffer(16, { maxByteLength: 16 });
    const i32 = new Int32Array(rab);
    const operand = {
      valueOf() {
        rab.resize(6);
        return 6;
      },
    };

    expect(() => Atomics.xor(i32, 1, operand)).toThrow(RangeError);
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

    expect(() => Atomics.xor(big, 7, operand)).toThrow(RangeError);
  });

  test("throws TypeError when the operand shrinks the buffer under a fixed-length view", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const i32 = new Int32Array(rab, 0, 16);
    const operand = {
      valueOf() {
        rab.resize(4);
        i32.length;
        return 6;
      },
    };

    expect(() => Atomics.xor(i32, 15, operand)).toThrow(TypeError);
  });

  test("throws TypeError when the operand detaches the buffer", () => {
    const rab = new ArrayBuffer(16, { maxByteLength: 16 });
    const i32 = new Int32Array(rab);
    const operand = {
      valueOf() {
        rab.transfer();
        return 6;
      },
    };

    expect(() => Atomics.xor(i32, 0, operand)).toThrow(TypeError);
  });

  test("writes the element in the buffer's current block after the operand grew the buffer", () => {
    const rab = new ArrayBuffer(4, { maxByteLength: 64 });
    const i32 = new Int32Array(rab);
    i32[0] = 1;
    const operand = {
      valueOf() {
        rab.resize(64);
        return 6;
      },
    };

    expect(Atomics.xor(i32, 0, operand)).toBe(1);
    expect(i32.length).toBe(16);
    expect(new Int32Array(rab)[0]).toBe(7);
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

    expect(() => Atomics.xor(i32, index, 6)).toThrow(TypeError);
    expect(converted).toBe(false);
  });
});
