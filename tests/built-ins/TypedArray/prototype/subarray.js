describe("TypedArray.prototype.subarray", () => {
  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("subarray shares buffer", () => {
      const ta = new TA([1, 2, 3, 4, 5]);
      const sub = ta.subarray(1, 4);
      expect(sub.length).toBe(3);
      expect(sub[0]).toBe(2);
      expect(sub[1]).toBe(3);
      expect(sub[2]).toBe(4);
      sub[0] = 9;
      expect(ta[1]).toBe(9);
    });

    test("returns instance of same type", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.subarray(0, 2)).toBeInstanceOf(TA);
    });

    test("negative indices", () => {
      const ta = new TA([1, 2, 3, 4]);
      const sub = ta.subarray(-2);
      expect(sub.length).toBe(2);
      expect(sub[0]).toBe(3);
      expect(sub[1]).toBe(4);
    });

    test("byteOffset reflects position", () => {
      const ta = new TA([1, 2, 3, 4]);
      const sub = ta.subarray(2);
      expect(sub.byteOffset).toBe(2 * TA.BYTES_PER_ELEMENT);
      expect(sub.length).toBe(2);
    });

    test("begin > end returns empty view", () => {
      const ta = new TA([1, 2, 3]);
      const sub = ta.subarray(2, 1);
      expect(sub.length).toBe(0);
    });

    test("both negative", () => {
      const ta = new TA([1, 2, 3, 4, 5]);
      const sub = ta.subarray(-3, -1);
      expect(sub.length).toBe(2);
      expect(sub[0]).toBe(3);
      expect(sub[1]).toBe(4);
    });

    test("begin beyond length returns empty", () => {
      const ta = new TA([1, 2, 3]);
      const sub = ta.subarray(10);
      expect(sub.length).toBe(0);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s subarray", (TA) => {
    const buf = new ArrayBuffer(32);
    const ta = new TA(buf);
    ta[0] = 10n; ta[1] = 20n; ta[2] = 30n; ta[3] = 40n;
    const sub = ta.subarray(1, 3);
    expect(sub.length).toBe(2);
    expect(sub[0]).toBe(20n);
    expect(sub[1]).toBe(30n);
    ta[1] = 99n;
    expect(sub[0]).toBe(99n);
  });

  test("uses constructor Symbol.species override", () => {
    const ta = new Uint16Array([0x0201, 0x0403, 0x0605]);
    ta.constructor = { [Symbol.species]: Uint8Array };

    const sub = ta.subarray(1, 3);

    expect(Object.prototype.toString.call(sub)).toBe("[object Uint8Array]");
    expect(sub.length).toBe(2);
    expect(sub.byteOffset).toBe(2);
    expect(sub[0]).toBe(3);
    expect(sub[1]).toBe(4);
  });

  test("throws when species view is not byte-aligned", () => {
    const ta = new Uint8Array([1, 2, 3, 4]);
    ta.constructor = { [Symbol.species]: Uint16Array };

    expect(() => ta.subarray(1, 2)).toThrow(RangeError);
  });

  test("omits length for length-tracking views when end is omitted", () => {
    const buf = new ArrayBuffer(8, { maxByteLength: 16 });
    const ta = new Uint8Array(buf, 2);
    let args;
    class Species extends Uint8Array {
      constructor(...params) {
        args = [params[0], params[1], params[2], params.length];
        super(params[0], params[1]);
      }
    }
    ta.constructor = {
      [Symbol.species]: Species,
    };

    const sub = ta.subarray(1);

    expect(args).toEqual([buf, 3, undefined, 2]);
    expect(sub.length).toBe(5);
  });

  test("length-tracking result keeps byteOffset after begin coercion grows buffer", () => {
    const buf = new ArrayBuffer(10, { maxByteLength: 10 });
    const ta = new Int8Array(buf, 4);
    buf.resize(0);

    const result = ta.subarray({
      valueOf() {
        buf.resize(10);
        return 1;
      },
    });

    expect(result.byteOffset).toBe(4);
    expect(result.length).toBe(6);
  });
});

describe("TypedArray.prototype.subarray non-finite range arguments", () => {
  test("Infinity begin yields an empty view", () => {
    expect(new Int8Array([1, 2]).subarray(Infinity).length).toBe(0);
  });

  test("-Infinity begin keeps the full view", () => {
    expect(new Int8Array([1, 2]).subarray(-Infinity).length).toBe(2);
  });
});
