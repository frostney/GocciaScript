describe("TypedArray element access", () => {
  describe("indexed read and write", () => {
    test("set and get elements", () => {
      const ta = new Int32Array(3);
      ta[0] = 10;
      ta[1] = 20;
      ta[2] = 30;
      expect(ta[0]).toBe(10);
      expect(ta[1]).toBe(20);
      expect(ta[2]).toBe(30);
    });

    test("out of bounds read returns undefined", () => {
      const ta = new Int32Array(2);
      expect(ta[2]).toBeUndefined();
      expect(ta[-1]).toBeUndefined();
      expect(ta[100]).toBeUndefined();
    });

    test("out of bounds write is silently ignored", () => {
      const ta = new Int32Array(2);
      ta[5] = 42;
      expect(ta[5]).toBeUndefined();
      expect(ta.length).toBe(2);
    });
  });

  describe("Int8Array range", () => {
    test("stores values in [-128, 127]", () => {
      const ta = new Int8Array(3);
      ta[0] = 127;
      ta[1] = -128;
      ta[2] = 0;
      expect(ta[0]).toBe(127);
      expect(ta[1]).toBe(-128);
      expect(ta[2]).toBe(0);
    });

    test("wraps on overflow", () => {
      const ta = new Int8Array(1);
      ta[0] = 128;
      expect(ta[0]).toBe(-128);
    });

    test("wraps on underflow", () => {
      const ta = new Int8Array(1);
      ta[0] = -129;
      expect(ta[0]).toBe(127);
    });
  });

  describe("Uint8Array range", () => {
    test("stores values in [0, 255]", () => {
      const ta = new Uint8Array(2);
      ta[0] = 0;
      ta[1] = 255;
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(255);
    });

    test("wraps on overflow", () => {
      const ta = new Uint8Array(1);
      ta[0] = 256;
      expect(ta[0]).toBe(0);
    });
  });

  describe("Uint8ClampedArray clamping", () => {
    test("clamps values above 255 to 255", () => {
      const ta = new Uint8ClampedArray(1);
      ta[0] = 300;
      expect(ta[0]).toBe(255);
    });

    test("clamps values below 0 to 0", () => {
      const ta = new Uint8ClampedArray(1);
      ta[0] = -10;
      expect(ta[0]).toBe(0);
    });

    test("rounds half-to-even", () => {
      const ta = new Uint8ClampedArray(4);
      ta[0] = 0.5;
      ta[1] = 1.5;
      ta[2] = 2.5;
      ta[3] = 3.5;
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(2);
      expect(ta[3]).toBe(4);
    });

    test("NaN becomes 0", () => {
      const ta = new Uint8ClampedArray(1);
      ta[0] = NaN;
      expect(ta[0]).toBe(0);
    });
  });

  describe("Int16Array range", () => {
    test("stores values in [-32768, 32767]", () => {
      const ta = new Int16Array(2);
      ta[0] = 32767;
      ta[1] = -32768;
      expect(ta[0]).toBe(32767);
      expect(ta[1]).toBe(-32768);
    });
  });

  describe("Uint16Array range", () => {
    test("stores values in [0, 65535]", () => {
      const ta = new Uint16Array(2);
      ta[0] = 0;
      ta[1] = 65535;
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(65535);
    });
  });

  describe("Int32Array range", () => {
    test("stores min and max", () => {
      const ta = new Int32Array(2);
      ta[0] = 2147483647;
      ta[1] = -2147483648;
      expect(ta[0]).toBe(2147483647);
      expect(ta[1]).toBe(-2147483648);
    });
  });

  describe("Uint32Array range", () => {
    test("stores 0 and max", () => {
      const ta = new Uint32Array(2);
      ta[0] = 0;
      ta[1] = 4294967295;
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(4294967295);
    });
  });

  describe("Float32Array precision", () => {
    test("stores float values with 32-bit precision", () => {
      const ta = new Float32Array(1);
      ta[0] = 1.5;
      expect(ta[0]).toBe(1.5);
    });

    test("rounds to 32-bit precision", () => {
      const ta = new Float32Array(1);
      ta[0] = Math.PI;
      expect(ta[0]).not.toBe(Math.PI);
      expect(Math.abs(ta[0] - Math.PI) < 0.0001).toBe(true);
    });
  });

  describe("Float64Array precision", () => {
    test("preserves full double precision", () => {
      const ta = new Float64Array(1);
      ta[0] = Math.PI;
      expect(ta[0]).toBe(Math.PI);
    });

    test("stores NaN", () => {
      const ta = new Float64Array(1);
      ta[0] = NaN;
      expect(Number.isNaN(ta[0])).toBe(true);
    });

    test("stores Infinity", () => {
      const ta = new Float64Array(2);
      ta[0] = Infinity;
      ta[1] = -Infinity;
      expect(ta[0]).toBe(Infinity);
      expect(ta[1]).toBe(-Infinity);
    });
  });

  describe("NaN written to integer types becomes 0", () => {
    test("Int8Array", () => {
      const ta = new Int8Array(1);
      ta[0] = NaN;
      expect(ta[0]).toBe(0);
    });

    test("Int32Array", () => {
      const ta = new Int32Array(1);
      ta[0] = NaN;
      expect(ta[0]).toBe(0);
    });

    test("Uint8Array", () => {
      const ta = new Uint8Array(1);
      ta[0] = NaN;
      expect(ta[0]).toBe(0);
    });

    test("Uint32Array", () => {
      const ta = new Uint32Array(1);
      ta[0] = NaN;
      expect(ta[0]).toBe(0);
    });
  });

  describe("Infinity written to integer types", () => {
    test("Int8Array truncates Infinity", () => {
      const ta = new Int8Array(1);
      ta[0] = Infinity;
      expect(ta[0]).toBe(0);
    });

    test("Uint8Array truncates Infinity", () => {
      const ta = new Uint8Array(1);
      ta[0] = Infinity;
      expect(ta[0]).toBe(0);
    });
  });

  describe("Float32Array special values", () => {
    test("stores NaN", () => {
      const ta = new Float32Array(1);
      ta[0] = NaN;
      expect(Number.isNaN(ta[0])).toBe(true);
    });

    test("stores Infinity", () => {
      const ta = new Float32Array(1);
      ta[0] = Infinity;
      expect(ta[0]).toBe(Infinity);
    });

    test("stores -Infinity", () => {
      const ta = new Float32Array(1);
      ta[0] = -Infinity;
      expect(ta[0]).toBe(-Infinity);
    });

    test("stores 0", () => {
      const ta = new Float32Array(1);
      ta[0] = 0;
      expect(ta[0]).toBe(0);
    });
  });

  describe("Uint8Array underflow wraps", () => {
    test("-1 wraps to 255", () => {
      const ta = new Uint8Array(1);
      ta[0] = -1;
      expect(ta[0]).toBe(255);
    });

    test("-256 wraps to 0", () => {
      const ta = new Uint8Array(1);
      ta[0] = -256;
      expect(ta[0]).toBe(0);
    });
  });

  describe("Int16Array overflow/underflow", () => {
    test("32768 wraps to -32768", () => {
      const ta = new Int16Array(1);
      ta[0] = 32768;
      expect(ta[0]).toBe(-32768);
    });

    test("-32769 wraps to 32767", () => {
      const ta = new Int16Array(1);
      ta[0] = -32769;
      expect(ta[0]).toBe(32767);
    });
  });

  describe("Uint16Array overflow", () => {
    test("65536 wraps to 0", () => {
      const ta = new Uint16Array(1);
      ta[0] = 65536;
      expect(ta[0]).toBe(0);
    });

    test("-1 wraps to 65535", () => {
      const ta = new Uint16Array(1);
      ta[0] = -1;
      expect(ta[0]).toBe(65535);
    });
  });

  describe("Uint8ClampedArray edge values", () => {
    test("Infinity clamps to 255", () => {
      const ta = new Uint8ClampedArray(1);
      ta[0] = Infinity;
      expect(ta[0]).toBe(255);
    });

    test("-Infinity clamps to 0", () => {
      const ta = new Uint8ClampedArray(1);
      ta[0] = -Infinity;
      expect(ta[0]).toBe(0);
    });

    test("exact boundary values", () => {
      const ta = new Uint8ClampedArray(4);
      ta[0] = 0;
      ta[1] = 255;
      ta[2] = -0.5;
      ta[3] = 255.5;
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(255);
      expect(ta[2]).toBe(0);
      expect(ta[3]).toBe(255);
    });
  });
});
