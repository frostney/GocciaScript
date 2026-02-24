describe("TypedArray constructors", () => {
  describe("all typed array constructors exist as globals", () => {
    test("Int8Array is defined", () => { expect(Int8Array).toBeDefined(); });
    test("Uint8Array is defined", () => { expect(Uint8Array).toBeDefined(); });
    test("Uint8ClampedArray is defined", () => { expect(Uint8ClampedArray).toBeDefined(); });
    test("Int16Array is defined", () => { expect(Int16Array).toBeDefined(); });
    test("Uint16Array is defined", () => { expect(Uint16Array).toBeDefined(); });
    test("Int32Array is defined", () => { expect(Int32Array).toBeDefined(); });
    test("Uint32Array is defined", () => { expect(Uint32Array).toBeDefined(); });
    test("Float32Array is defined", () => { expect(Float32Array).toBeDefined(); });
    test("Float64Array is defined", () => { expect(Float64Array).toBeDefined(); });
  });

  describe("new TypedArray() — zero-length", () => {
    test("new Int8Array()", () => {
      const ta = new Int8Array();
      expect(ta.length).toBe(0);
      expect(ta.byteLength).toBe(0);
      expect(ta.byteOffset).toBe(0);
    });
    test("new Uint8Array()", () => {
      const ta = new Uint8Array();
      expect(ta.length).toBe(0);
    });
    test("new Float64Array()", () => {
      const ta = new Float64Array();
      expect(ta.length).toBe(0);
    });
  });

  describe("new TypedArray(length)", () => {
    test("new Int8Array(4)", () => {
      const ta = new Int8Array(4);
      expect(ta.length).toBe(4);
      expect(ta.byteLength).toBe(4);
    });
    test("new Int16Array(4)", () => {
      const ta = new Int16Array(4);
      expect(ta.length).toBe(4);
      expect(ta.byteLength).toBe(8);
    });
    test("new Int32Array(4)", () => {
      const ta = new Int32Array(4);
      expect(ta.length).toBe(4);
      expect(ta.byteLength).toBe(16);
    });
    test("new Float64Array(4)", () => {
      const ta = new Float64Array(4);
      expect(ta.length).toBe(4);
      expect(ta.byteLength).toBe(32);
    });
    test("elements are initialized to 0", () => {
      const ta = new Int32Array(3);
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(0);
      expect(ta[2]).toBe(0);
    });
  });

  describe("new TypedArray(array)", () => {
    test("Int32Array from array literal", () => {
      const ta = new Int32Array([10, 20, 30]);
      expect(ta.length).toBe(3);
      expect(ta[0]).toBe(10);
      expect(ta[1]).toBe(20);
      expect(ta[2]).toBe(30);
    });
    test("Float64Array from array literal", () => {
      const ta = new Float64Array([1.5, 2.5, 3.5]);
      expect(ta.length).toBe(3);
      expect(ta[0]).toBe(1.5);
      expect(ta[1]).toBe(2.5);
      expect(ta[2]).toBe(3.5);
    });
  });

  describe("new TypedArray(typedArray)", () => {
    test("copy from same type", () => {
      const src = new Int32Array([1, 2, 3]);
      const copy = new Int32Array(src);
      expect(copy.length).toBe(3);
      expect(copy[0]).toBe(1);
      expect(copy[1]).toBe(2);
      expect(copy[2]).toBe(3);
      src[0] = 99;
      expect(copy[0]).toBe(1);
    });
    test("copy across types with conversion", () => {
      const f64 = new Float64Array([1.7, 2.3, 3.9]);
      const i32 = new Int32Array(f64);
      expect(i32[0]).toBe(1);
      expect(i32[1]).toBe(2);
      expect(i32[2]).toBe(3);
    });
  });

  describe("new TypedArray(buffer [, byteOffset [, length]])", () => {
    test("view entire buffer", () => {
      const buf = new ArrayBuffer(8);
      const view = new Int32Array(buf);
      expect(view.length).toBe(2);
      expect(view.byteLength).toBe(8);
      expect(view.byteOffset).toBe(0);
      expect(view.buffer).toBe(buf);
    });
    test("view with byte offset", () => {
      const buf = new ArrayBuffer(12);
      const view = new Int32Array(buf, 4);
      expect(view.length).toBe(2);
      expect(view.byteLength).toBe(8);
      expect(view.byteOffset).toBe(4);
    });
    test("view with byte offset and length", () => {
      const buf = new ArrayBuffer(16);
      const view = new Int32Array(buf, 4, 2);
      expect(view.length).toBe(2);
      expect(view.byteLength).toBe(8);
      expect(view.byteOffset).toBe(4);
    });
    test("shared buffer between views", () => {
      const buf = new ArrayBuffer(4);
      const u8 = new Uint8Array(buf);
      const i32 = new Int32Array(buf);
      i32[0] = 0x04030201;
      expect(u8[0]).toBe(1);
      expect(u8[1]).toBe(2);
      expect(u8[2]).toBe(3);
      expect(u8[3]).toBe(4);
    });
    test("unaligned offset throws RangeError", () => {
      const buf = new ArrayBuffer(8);
      expect(() => new Int32Array(buf, 1)).toThrow(RangeError);
    });
    test("buffer too small for type throws RangeError", () => {
      const buf = new ArrayBuffer(3);
      expect(() => new Int32Array(buf)).toThrow(RangeError);
    });
    test("offset beyond buffer throws RangeError", () => {
      const buf = new ArrayBuffer(4);
      expect(() => new Int32Array(buf, 8)).toThrow(RangeError);
    });
    test("length exceeds buffer bounds throws RangeError", () => {
      const buf = new ArrayBuffer(8);
      expect(() => new Int32Array(buf, 0, 3)).toThrow(RangeError);
    });
  });

  describe("BYTES_PER_ELEMENT", () => {
    test("Int8Array.BYTES_PER_ELEMENT === 1", () => { expect(Int8Array.BYTES_PER_ELEMENT).toBe(1); });
    test("Uint8Array.BYTES_PER_ELEMENT === 1", () => { expect(Uint8Array.BYTES_PER_ELEMENT).toBe(1); });
    test("Uint8ClampedArray.BYTES_PER_ELEMENT === 1", () => { expect(Uint8ClampedArray.BYTES_PER_ELEMENT).toBe(1); });
    test("Int16Array.BYTES_PER_ELEMENT === 2", () => { expect(Int16Array.BYTES_PER_ELEMENT).toBe(2); });
    test("Uint16Array.BYTES_PER_ELEMENT === 2", () => { expect(Uint16Array.BYTES_PER_ELEMENT).toBe(2); });
    test("Int32Array.BYTES_PER_ELEMENT === 4", () => { expect(Int32Array.BYTES_PER_ELEMENT).toBe(4); });
    test("Uint32Array.BYTES_PER_ELEMENT === 4", () => { expect(Uint32Array.BYTES_PER_ELEMENT).toBe(4); });
    test("Float32Array.BYTES_PER_ELEMENT === 4", () => { expect(Float32Array.BYTES_PER_ELEMENT).toBe(4); });
    test("Float64Array.BYTES_PER_ELEMENT === 8", () => { expect(Float64Array.BYTES_PER_ELEMENT).toBe(8); });

    test("instance BYTES_PER_ELEMENT", () => {
      expect(new Int8Array(0).BYTES_PER_ELEMENT).toBe(1);
      expect(new Int32Array(0).BYTES_PER_ELEMENT).toBe(4);
      expect(new Float64Array(0).BYTES_PER_ELEMENT).toBe(8);
    });
  });

  describe("negative length throws RangeError", () => {
    test("new Int32Array(-1) throws", () => {
      expect(() => new Int32Array(-1)).toThrow(RangeError);
    });
  });

  describe("constructor edge cases", () => {
    test("new Int32Array(0) creates empty array", () => {
      const ta = new Int32Array(0);
      expect(ta.length).toBe(0);
      expect(ta.byteLength).toBe(0);
    });

    test("new Int32Array(NaN) creates empty array", () => {
      const ta = new Int32Array(NaN);
      expect(ta.length).toBe(0);
    });

    test("new Int32Array([]) creates empty array", () => {
      const ta = new Int32Array([]);
      expect(ta.length).toBe(0);
    });

    test("new Int32Array from empty typed array", () => {
      const src = new Int32Array(0);
      const copy = new Int32Array(src);
      expect(copy.length).toBe(0);
    });

    test("new Int16Array(buf, 1) unaligned throws RangeError", () => {
      const buf = new ArrayBuffer(4);
      expect(() => new Int16Array(buf, 1)).toThrow(RangeError);
    });

    test("new Float64Array(buf, 3) unaligned throws RangeError", () => {
      const buf = new ArrayBuffer(16);
      expect(() => new Float64Array(buf, 3)).toThrow(RangeError);
    });

    test("new Uint16Array(buf) with odd byte length throws RangeError", () => {
      const buf = new ArrayBuffer(3);
      expect(() => new Uint16Array(buf)).toThrow(RangeError);
    });

    test("new Float64Array(buf) with non-multiple-of-8 length throws RangeError", () => {
      const buf = new ArrayBuffer(7);
      expect(() => new Float64Array(buf)).toThrow(RangeError);
    });

    test("buffer view at offset 0 with length 0", () => {
      const buf = new ArrayBuffer(8);
      const ta = new Int32Array(buf, 0, 0);
      expect(ta.length).toBe(0);
      expect(ta.byteLength).toBe(0);
      expect(ta.byteOffset).toBe(0);
      expect(ta.buffer).toBe(buf);
    });

    test("buffer view with offset at end of buffer and length 0", () => {
      const buf = new ArrayBuffer(8);
      const ta = new Int32Array(buf, 8, 0);
      expect(ta.length).toBe(0);
      expect(ta.byteOffset).toBe(8);
    });
  });
});
