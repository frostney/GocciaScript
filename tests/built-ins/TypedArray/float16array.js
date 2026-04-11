describe("Float16Array", () => {
  test("Float16Array is defined", () => {
    expect(Float16Array).toBeDefined();
  });

  test("BYTES_PER_ELEMENT is 2", () => {
    expect(Float16Array.BYTES_PER_ELEMENT).toBe(2);
  });

  test("instance BYTES_PER_ELEMENT is 2", () => {
    expect(new Float16Array(0).BYTES_PER_ELEMENT).toBe(2);
  });

  describe("construction", () => {
    test("new Float16Array() creates zero-length", () => {
      const ta = new Float16Array();
      expect(ta.length).toBe(0);
      expect(ta.byteLength).toBe(0);
      expect(ta.byteOffset).toBe(0);
    });

    test("new Float16Array(length)", () => {
      const ta = new Float16Array(4);
      expect(ta.length).toBe(4);
      expect(ta.byteLength).toBe(8);
    });

    test("elements are initialized to 0", () => {
      const ta = new Float16Array(3);
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(0);
      expect(ta[2]).toBe(0);
    });

    test("new Float16Array(array)", () => {
      const ta = new Float16Array([1.5, 2.5, 3.5]);
      expect(ta.length).toBe(3);
      expect(ta[0]).toBe(1.5);
      expect(ta[1]).toBe(2.5);
      expect(ta[2]).toBe(3.5);
    });

    test("new Float16Array(typedArray) — copy from same type", () => {
      const src = new Float16Array([1.5, 2.5]);
      const copy = new Float16Array(src);
      expect(copy.length).toBe(2);
      expect(copy[0]).toBe(1.5);
      expect(copy[1]).toBe(2.5);
      src[0] = 99;
      expect(copy[0]).toBe(1.5);
    });

    test("new Float16Array(typedArray) — cross-type conversion", () => {
      const f64 = new Float64Array([1.5, 2.5, 3.5]);
      const f16 = new Float16Array(f64);
      expect(f16[0]).toBe(1.5);
      expect(f16[1]).toBe(2.5);
      expect(f16[2]).toBe(3.5);
    });

    test("new Float16Array(buffer)", () => {
      const buf = new ArrayBuffer(6);
      const ta = new Float16Array(buf);
      expect(ta.length).toBe(3);
      expect(ta.byteLength).toBe(6);
      expect(ta.byteOffset).toBe(0);
      expect(ta.buffer).toBe(buf);
    });

    test("new Float16Array(buffer, byteOffset)", () => {
      const buf = new ArrayBuffer(8);
      const ta = new Float16Array(buf, 2);
      expect(ta.length).toBe(3);
      expect(ta.byteLength).toBe(6);
      expect(ta.byteOffset).toBe(2);
    });

    test("new Float16Array(buffer, byteOffset, length)", () => {
      const buf = new ArrayBuffer(8);
      const ta = new Float16Array(buf, 2, 2);
      expect(ta.length).toBe(2);
      expect(ta.byteLength).toBe(4);
      expect(ta.byteOffset).toBe(2);
    });

    test("unaligned offset throws RangeError", () => {
      const buf = new ArrayBuffer(8);
      expect(() => new Float16Array(buf, 1)).toThrow(RangeError);
    });

    test("buffer too small throws RangeError", () => {
      const buf = new ArrayBuffer(1);
      expect(() => new Float16Array(buf)).toThrow(RangeError);
    });

    test("negative length throws RangeError", () => {
      expect(() => new Float16Array(-1)).toThrow(RangeError);
    });

    test("new Float16Array(NaN) creates empty array", () => {
      const ta = new Float16Array(NaN);
      expect(ta.length).toBe(0);
    });
  });

  describe("precision and range", () => {
    test("max finite value is 65504", () => {
      const ta = new Float16Array(1);
      ta[0] = 65504;
      expect(ta[0]).toBe(65504);
    });

    test("overflow to Infinity", () => {
      const ta = new Float16Array(1);
      ta[0] = 65520;
      expect(ta[0]).toBe(Infinity);
    });

    test("stores NaN", () => {
      const ta = new Float16Array(1);
      ta[0] = NaN;
      expect(Number.isNaN(ta[0])).toBe(true);
    });

    test("stores Infinity", () => {
      const ta = new Float16Array(1);
      ta[0] = Infinity;
      expect(ta[0]).toBe(Infinity);
    });

    test("stores -Infinity", () => {
      const ta = new Float16Array(1);
      ta[0] = -Infinity;
      expect(ta[0]).toBe(-Infinity);
    });

    test("stores negative zero", () => {
      const ta = new Float16Array(1);
      ta[0] = -0;
      expect(Object.is(ta[0], -0)).toBe(true);
    });

    test("min positive normal: 2^(-14)", () => {
      const ta = new Float16Array(1);
      ta[0] = 0.00006103515625;
      expect(ta[0]).toBe(0.00006103515625);
    });

    test("min positive subnormal: 2^(-24)", () => {
      const ta = new Float16Array(1);
      ta[0] = 5.960464477539063e-8;
      expect(ta[0]).toBeCloseTo(5.960464477539063e-8, 15);
    });

    test("values smaller than min subnormal round to zero", () => {
      const ta = new Float16Array(1);
      ta[0] = 1e-10;
      expect(ta[0]).toBe(0);
    });

    test("half-precision rounding (round to nearest even)", () => {
      const ta = new Float16Array(1);
      // 1.0 + 2^(-10) is representable
      ta[0] = 1.0009765625;
      expect(ta[0]).toBe(1.0009765625);
      // 1.0 + 2^(-11) rounds to 1.0 (tie, mantissa 0 is even)
      ta[0] = 1.00048828125;
      expect(ta[0]).toBe(1);
    });

    test("negative values", () => {
      const ta = new Float16Array([- 1.5, -2.5, -65504]);
      expect(ta[0]).toBe(-1.5);
      expect(ta[1]).toBe(-2.5);
      expect(ta[2]).toBe(-65504);
    });
  });

  describe("buffer sharing", () => {
    test("shared buffer between Float16Array and Uint8Array", () => {
      const buf = new ArrayBuffer(2);
      const f16 = new Float16Array(buf);
      const u8 = new Uint8Array(buf);
      f16[0] = 1.0;
      // IEEE 754 half-precision 1.0 = 0x3C00 (little-endian: 0x00, 0x3C)
      expect(u8[0]).toBe(0);
      expect(u8[1]).toBe(0x3C);
    });

    test("shared buffer between Float16Array and Uint16Array", () => {
      const buf = new ArrayBuffer(4);
      const f16 = new Float16Array(buf);
      const u16 = new Uint16Array(buf);
      f16[0] = 1.0;
      f16[1] = -1.0;
      // 1.0 = 0x3C00, -1.0 = 0xBC00
      expect(u16[0]).toBe(0x3C00);
      expect(u16[1]).toBe(0xBC00);
    });
  });

  describe("static methods", () => {
    test("Float16Array.from(array)", () => {
      const ta = Float16Array.from([1, 2, 3]);
      expect(ta.length).toBe(3);
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(3);
    });

    test("Float16Array.of(...elements)", () => {
      const ta = Float16Array.of(1.5, 2.5, 3.5);
      expect(ta.length).toBe(3);
      expect(ta[0]).toBe(1.5);
      expect(ta[1]).toBe(2.5);
      expect(ta[2]).toBe(3.5);
    });
  });

  describe("prototype methods", () => {
    test("at()", () => {
      const ta = new Float16Array([10, 20, 30]);
      expect(ta.at(0)).toBe(10);
      expect(ta.at(-1)).toBe(30);
    });

    test("slice()", () => {
      const ta = new Float16Array([1, 2, 3, 4]);
      const sliced = ta.slice(1, 3);
      expect(sliced.length).toBe(2);
      expect(sliced[0]).toBe(2);
      expect(sliced[1]).toBe(3);
    });

    test("fill()", () => {
      const ta = new Float16Array(3);
      ta.fill(7);
      expect(ta[0]).toBe(7);
      expect(ta[1]).toBe(7);
      expect(ta[2]).toBe(7);
    });

    test("indexOf() and includes()", () => {
      const ta = new Float16Array([10, 20, 30]);
      expect(ta.indexOf(20)).toBe(1);
      expect(ta.indexOf(99)).toBe(-1);
      expect(ta.includes(30)).toBe(true);
      expect(ta.includes(99)).toBe(false);
    });

    test("indexOf()/lastIndexOf()/includes() handle float special values", () => {
      const ta = new Float16Array([NaN, Infinity, -Infinity, 1]);
      // includes uses SameValueZero: NaN === NaN
      expect(ta.includes(NaN)).toBe(true);
      // indexOf uses strict equality: NaN !== NaN
      expect(ta.indexOf(NaN)).toBe(-1);
      expect(ta.indexOf(Infinity)).toBe(1);
      expect(ta.lastIndexOf(-Infinity)).toBe(2);
      expect(ta.includes(Infinity)).toBe(true);
      expect(ta.includes(-Infinity)).toBe(true);
    });

    test("map()", () => {
      const ta = new Float16Array([1, 2, 3]);
      const mapped = ta.map((x) => x * 2);
      expect(mapped[0]).toBe(2);
      expect(mapped[1]).toBe(4);
      expect(mapped[2]).toBe(6);
    });

    test("filter()", () => {
      const ta = new Float16Array([1, 2, 3, 4]);
      const filtered = ta.filter((x) => x > 2);
      expect(filtered.length).toBe(2);
      expect(filtered[0]).toBe(3);
      expect(filtered[1]).toBe(4);
    });

    test("reduce()", () => {
      const ta = new Float16Array([1, 2, 3]);
      const sum = ta.reduce((acc, x) => acc + x, 0);
      expect(sum).toBe(6);
    });

    test("sort()", () => {
      const ta = new Float16Array([3, 1, 2]);
      ta.sort();
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(3);
    });

    test("sort() moves NaN values to the end", () => {
      const ta = new Float16Array([3, NaN, 1, NaN, 2]);
      ta.sort();
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(3);
      expect(Number.isNaN(ta[3])).toBe(true);
      expect(Number.isNaN(ta[4])).toBe(true);
    });

    test("reverse()", () => {
      const ta = new Float16Array([1, 2, 3]);
      ta.reverse();
      expect(ta[0]).toBe(3);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(1);
    });

    test("join()", () => {
      const ta = new Float16Array([1, 2, 3]);
      expect(ta.join(",")).toBe("1,2,3");
    });

    test("toString()", () => {
      const ta = new Float16Array([1, 2, 3]);
      expect(ta.toString()).toBe("1,2,3");
    });

    test("every() and some()", () => {
      const ta = new Float16Array([2, 4, 6]);
      expect(ta.every((x) => x % 2 === 0)).toBe(true);
      expect(ta.some((x) => x > 5)).toBe(true);
      expect(ta.some((x) => x > 10)).toBe(false);
    });

    test("find() and findIndex()", () => {
      const ta = new Float16Array([1, 2, 3, 4]);
      expect(ta.find((x) => x > 2)).toBe(3);
      expect(ta.findIndex((x) => x > 2)).toBe(2);
    });

    test("keys(), values(), entries()", () => {
      const ta = new Float16Array([10, 20]);
      const keys = [...ta.keys()];
      expect(keys[0]).toBe(0);
      expect(keys[1]).toBe(1);

      const values = [...ta.values()];
      expect(values[0]).toBe(10);
      expect(values[1]).toBe(20);

      const entries = [...ta.entries()];
      expect(entries[0][0]).toBe(0);
      expect(entries[0][1]).toBe(10);
      expect(entries[1][0]).toBe(1);
      expect(entries[1][1]).toBe(20);
    });

    test("set()", () => {
      const ta = new Float16Array(4);
      ta.set([10, 20], 1);
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(10);
      expect(ta[2]).toBe(20);
      expect(ta[3]).toBe(0);
    });

    test("subarray()", () => {
      const ta = new Float16Array([1, 2, 3, 4]);
      const sub = ta.subarray(1, 3);
      expect(sub.length).toBe(2);
      expect(sub[0]).toBe(2);
      expect(sub[1]).toBe(3);
      // subarray shares the same buffer
      sub[0] = 99;
      expect(ta[1]).toBe(99);
    });

    test("copyWithin()", () => {
      const ta = new Float16Array([1, 2, 3, 4, 5]);
      ta.copyWithin(0, 3);
      expect(ta[0]).toBe(4);
      expect(ta[1]).toBe(5);
    });

    test("forEach()", () => {
      const ta = new Float16Array([1, 2, 3]);
      const result = [];
      ta.forEach((v) => result.push(v));
      expect(result.length).toBe(3);
      expect(result[0]).toBe(1);
      expect(result[1]).toBe(2);
      expect(result[2]).toBe(3);
    });
  });
});
