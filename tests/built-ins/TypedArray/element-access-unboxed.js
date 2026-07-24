// Regression coverage for unboxed typed-array element reads and writes: the
// results must stay observably identical in both execution modes.

describe("TypedArray unboxed element fast path", () => {
  describe("signed zero round-trips", () => {
    test("float array preserves -0 read back through an index", () => {
      const ta = new Float64Array(1);
      ta[0] = -0;
      expect(Object.is(ta[0], -0)).toBe(true);
      expect(Object.is(ta[0], 0)).toBe(false);
    });

    test("float32 array preserves -0", () => {
      const ta = new Float32Array(1);
      ta[0] = -0;
      expect(Object.is(ta[0], -0)).toBe(true);
    });

    test("integer array normalizes -0 to +0", () => {
      const ta = new Int32Array(1);
      ta[0] = -0;
      expect(Object.is(ta[0], 0)).toBe(true);
      expect(Object.is(ta[0], -0)).toBe(false);
    });
  });

  describe("unboxed reads feed comparisons and equality", () => {
    test("strict equality against a number literal", () => {
      const ta = new Int16Array([-32768, 0, 32767]);
      expect(ta[0] === -32768).toBe(true);
      expect(ta[1] === 0).toBe(true);
      expect(ta[2] === 32767).toBe(true);
      expect(ta[0] === 0).toBe(false);
    });

    test("relational comparison of two elements (counting-sort scan shape)", () => {
      const ta = new Uint16Array([0, 1, 1, 7, 65535]);
      let sorted = true;
      let scanned = 0;
      // for...of so the workload actually runs (traditional for is opt-in here).
      [0, 1, 2, 3].forEach((i) => {
        scanned += 1;
        if (ta[i] > ta[i + 1]) sorted = false;
      });
      expect(scanned).toBe(4);
      expect(sorted).toBe(true);
    });

    test("element used directly in arithmetic stays unboxed-correct", () => {
      const ta = new Int32Array([10, 20, 30]);
      expect(ta[0] + ta[1] + ta[2]).toBe(60);
      expect(ta[2] - ta[0]).toBe(20);
    });

    test("float NaN read compares as not-equal to itself", () => {
      const ta = new Float64Array(1);
      ta[0] = NaN;
      expect(ta[0] === ta[0]).toBe(false);
      expect(Number.isNaN(ta[0])).toBe(true);
    });

    test("float Infinity read compares correctly", () => {
      const ta = new Float32Array([Infinity, -Infinity]);
      expect(ta[0] > 0).toBe(true);
      expect(ta[1] < 0).toBe(true);
      expect(ta[0] === Infinity).toBe(true);
    });
  });

  describe("writes from a variable (register-resident scalar)", () => {
    test("integer value held in a let binding", () => {
      const ta = new Int8Array(3);
      let v = 127;
      ta[0] = v;
      ta[1] = v - 255;
      ta[2] = v + 1;
      expect(ta[0]).toBe(127);
      expect(ta[1]).toBe(-128);
      expect(ta[2]).toBe(-128);
    });

    test("float value held in a let binding", () => {
      const ta = new Float64Array(1);
      let v = 3.5;
      ta[0] = v;
      expect(ta[0]).toBe(3.5);
    });

    test("computed index from a variable", () => {
      const ta = new Uint16Array(4);
      [0, 1, 2, 3].forEach((i) => {
        ta[i] = i * 100;
      });
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(100);
      expect(ta[3]).toBe(300);
    });
  });

  describe("fixed-length backing buffer specialization", () => {
    test("reads and writes a fixed-length offset view through shared storage", () => {
      const buffer = new ArrayBuffer(8);
      const bytes = new Uint8Array(buffer);
      const words = new Uint16Array(buffer, 2, 2);

      words[0] = 0x1234;
      words[1] = 0xabcd;

      expect(words[0]).toBe(0x1234);
      expect(words[1]).toBe(0xabcd);
      expect(bytes[2]).toBe(0x34);
      expect(bytes[3]).toBe(0x12);
      expect(bytes[4]).toBe(0xcd);
      expect(bytes[5]).toBe(0xab);
    });

    test("checks detachment before every fixed-length read and write", () => {
      const buffer = new ArrayBuffer(4);
      const view = new Uint8Array(buffer);
      view[0] = 9;

      buffer.transfer();

      expect(view[0]).toBeUndefined();
      view[0] = 7;
      expect(view[0]).toBeUndefined();
      expect(view.length).toBe(0);
    });

    test("preserves out-of-bounds results on fixed-length storage", () => {
      const view = new Uint8Array(2);
      view[2] = 7;
      view[-1] = 8;

      expect(view[2]).toBeUndefined();
      expect(view[-1]).toBeUndefined();
      expect(view.length).toBe(2);
    });
  });

  describe("indexed access fallback paths", () => {
    test("revalidates a fixed-length view over a resizable buffer", () => {
      const buffer = new ArrayBuffer(4, { maxByteLength: 8 });
      const view = new Uint8Array(buffer, 1, 2);
      view[0] = 11;
      view[1] = 22;

      buffer.resize(1);
      expect(view[0]).toBeUndefined();
      view[0] = 33;
      expect(view[0]).toBeUndefined();

      buffer.resize(4);
      view[0] = 44;
      expect(view[0]).toBe(44);
    });

    test("keeps length-tracking resizable views synchronized", () => {
      const buffer = new ArrayBuffer(1, { maxByteLength: 4 });
      const view = new Uint8Array(buffer);
      view[0] = 5;

      buffer.resize(3);
      view[2] = 9;

      expect(view.length).toBe(3);
      expect(view[0]).toBe(5);
      expect(view[2]).toBe(9);
    });

    test("keeps SharedArrayBuffer-backed access on the general path", () => {
      const buffer = new SharedArrayBuffer(4);
      const view = new Uint8Array(buffer);

      view[0] = 17;
      expect(view[0]).toBe(17);
    });

    test("keeps immutable-buffer writes on the general path", () => {
      const mutable = new ArrayBuffer(2);
      const seed = new Uint8Array(mutable);
      seed[0] = 23;
      const view = new Uint8Array(mutable.transferToImmutable());

      view[0] = 99;
      expect(view[0]).toBe(23);
    });

    test("preserves computed-key then value conversion order", () => {
      const order = [];
      const view = new Uint8Array(1);
      const key = {
        toString() {
          order.push("key");
          return "0";
        },
      };
      const value = {
        valueOf() {
          order.push("value");
          return 31;
        },
      };

      view[key] = value;

      expect(order).toEqual(["key", "value"]);
      expect(view[0]).toBe(31);
    });

    test("propagates numeric conversion exceptions", () => {
      const view = new Uint8Array(1);
      const value = {
        valueOf() {
          throw new Error("conversion failed");
        },
      };

      expect(() => {
        view[0] = value;
      }).toThrow("conversion failed");
      expect(view[0]).toBe(0);
    });
  });

  describe("Float16Array index access", () => {
    test("round-trips representable half-precision values", () => {
      const ta = new Float16Array(3);
      ta[0] = 1.5;
      ta[1] = -2;
      ta[2] = 0.5;
      expect(ta[0]).toBe(1.5);
      expect(ta[1]).toBe(-2);
      expect(ta[2]).toBe(0.5);
    });

    test("stores and reads -Infinity (counting-sort smallest)", () => {
      const ta = new Float16Array(1);
      ta[0] = -Infinity;
      expect(ta[0]).toBe(-Infinity);
    });
  });

  describe("non-scalar values still coerce via the slow path", () => {
    test("boolean value coerces with ToNumber", () => {
      const ta = new Int32Array(2);
      ta[0] = true;
      ta[1] = false;
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(0);
    });

    test("null and undefined coerce with ToNumber", () => {
      const intArr = new Int32Array(2);
      intArr[0] = null;
      intArr[1] = undefined;
      expect(intArr[0]).toBe(0);
      expect(intArr[1]).toBe(0);

      const floatArr = new Float64Array(1);
      floatArr[0] = undefined;
      expect(Number.isNaN(floatArr[0])).toBe(true);
    });

    test("object with valueOf coerces with ToNumber", () => {
      const ta = new Uint8Array(1);
      ta[0] = { valueOf: () => 200 };
      expect(ta[0]).toBe(200);
    });

    test("number value into a BigInt array still throws TypeError", () => {
      const ta = new BigInt64Array(1);
      expect(() => { ta[0] = 5; }).toThrow(TypeError);
    });
  });

  describe("large array index access (counting-sort scale)", () => {
    test("fill, mutate the middle, and read back across a big buffer", () => {
      const len = 1 << 16;
      const ta = new Int16Array(len);
      ta.fill(-32768);
      const offset = 10000;
      const indices = Array.from({ length: 256 }, (_, i) => i);
      indices.forEach((i) => {
        ta[offset + i] = i;
      });
      expect(ta[0]).toBe(-32768);
      expect(ta[offset]).toBe(0);
      expect(ta[offset + 255]).toBe(255);
      expect(ta[len - 1]).toBe(-32768);
    });
  });
});
