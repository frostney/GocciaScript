describe("Atomics.compareExchange", () => {
  test("writes the replacement only when the expected value matches", () => {
    const i32 = new Int32Array(new SharedArrayBuffer(4));
    i32[0] = 10;

    expect(Atomics.compareExchange(i32, 0, 5, 20)).toBe(10);
    expect(i32[0]).toBe(10);
    expect(Atomics.compareExchange(i32, 0, 10, 20)).toBe(10);
    expect(i32[0]).toBe(20);
  });

  test("validates the target and index", () => {
    expect(() => Atomics.compareExchange(new Float32Array(new SharedArrayBuffer(4)), 0, 0, 1)).toThrow(TypeError);
    expect(() => Atomics.compareExchange(new Int32Array(new SharedArrayBuffer(4)), 1, 0, 1)).toThrow(RangeError);
  });

  test("converts both operands before it reads the element", () => {
    const i32 = new Int32Array(4);
    i32[0] = 1;
    const replacement = {
      valueOf() {
        i32[0] = 10;
        return 5;
      },
    };

    expect(Atomics.compareExchange(i32, 0, 10, replacement)).toBe(10);
    expect(i32[0]).toBe(5);
  });

  test("converts the index, the expected value and the replacement in that order", () => {
    const i32 = new Int32Array(4);
    const order = [];
    const record = (name, value) => ({
      valueOf() {
        order.push(name);
        return value;
      },
    });

    Atomics.compareExchange(i32, record("index", 0), record("expected", 0), record("replacement", 0));
    expect(order).toEqual(["index", "expected", "replacement"]);
  });

  test("throws RangeError and writes nothing when the replacement shrinks a length-tracking view past the element", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab);
    const replacement = {
      valueOf() {
        rab.resize(4);
        u8.length;
        return 1;
      },
    };

    expect(() => Atomics.compareExchange(u8, 60, 0, replacement)).toThrow(RangeError);
    expect(Array.from(u8)).toEqual([0, 0, 0, 0]);
  });

  test("throws RangeError when the expected value shrinks a BigInt64Array's buffer past the element", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const big = new BigInt64Array(rab);
    const expected = {
      valueOf() {
        rab.resize(8);
        big.length;
        return 0n;
      },
    };

    expect(() => Atomics.compareExchange(big, 7, expected, 1n)).toThrow(RangeError);
  });

  test("throws TypeError when the replacement shrinks the buffer under a fixed-length view", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const i32 = new Int32Array(rab, 0, 16);
    const replacement = {
      valueOf() {
        rab.resize(4);
        i32.length;
        return 1;
      },
    };

    expect(() => Atomics.compareExchange(i32, 15, 0, replacement)).toThrow(TypeError);
  });

  test("throws TypeError when the expected value detaches the buffer", () => {
    const rab = new ArrayBuffer(16, { maxByteLength: 16 });
    const i32 = new Int32Array(rab);
    const expected = {
      valueOf() {
        rab.transfer();
        return 0;
      },
    };

    expect(() => Atomics.compareExchange(i32, 0, expected, 1)).toThrow(TypeError);
  });

  test("exchanges the element in the buffer's current block after the replacement grew the buffer", () => {
    const rab = new ArrayBuffer(4, { maxByteLength: 64 });
    const i32 = new Int32Array(rab);
    const replacement = {
      valueOf() {
        rab.resize(64);
        return 7;
      },
    };

    expect(Atomics.compareExchange(i32, 0, 0, replacement)).toBe(0);
    expect(new Int32Array(rab)[0]).toBe(7);
  });

  test("throws TypeError for a view past the end of its shrunk buffer", () => {
    const rab = new ArrayBuffer(64, { maxByteLength: 64 });
    const u8 = new Uint8Array(rab, 0, 32);
    rab.resize(4);

    expect(() => Atomics.compareExchange(u8, 0, 0, 1)).toThrow(TypeError);
  });
});
