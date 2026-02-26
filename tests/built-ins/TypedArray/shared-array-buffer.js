describe("TypedArray with SharedArrayBuffer", () => {
  describe("construction from SharedArrayBuffer", () => {
    test("creates a view on SharedArrayBuffer", () => {
      const sab = new SharedArrayBuffer(16);
      const ta = new Int32Array(sab);
      expect(ta.length).toBe(4);
      expect(ta.byteLength).toBe(16);
      expect(ta.byteOffset).toBe(0);
    });

    test(".buffer returns the original SharedArrayBuffer", () => {
      const sab = new SharedArrayBuffer(8);
      const ta = new Float64Array(sab);
      expect(ta.buffer).toBe(sab);
      expect(ta.buffer instanceof SharedArrayBuffer).toBe(true);
    });

    test("with byteOffset", () => {
      const sab = new SharedArrayBuffer(16);
      const ta = new Int32Array(sab, 4);
      expect(ta.length).toBe(3);
      expect(ta.byteOffset).toBe(4);
      expect(ta.byteLength).toBe(12);
    });

    test("with byteOffset and length", () => {
      const sab = new SharedArrayBuffer(16);
      const ta = new Int32Array(sab, 4, 2);
      expect(ta.length).toBe(2);
      expect(ta.byteOffset).toBe(4);
      expect(ta.byteLength).toBe(8);
    });

    test("throws RangeError for misaligned byteOffset", () => {
      const sab = new SharedArrayBuffer(16);
      expect(() => new Int32Array(sab, 3)).toThrow(RangeError);
    });

    test("throws RangeError for out-of-bounds offset", () => {
      const sab = new SharedArrayBuffer(8);
      expect(() => new Int32Array(sab, 12)).toThrow(RangeError);
    });

    test("throws RangeError when length exceeds buffer", () => {
      const sab = new SharedArrayBuffer(8);
      expect(() => new Int32Array(sab, 0, 10)).toThrow(RangeError);
    });
  });

  describe("read and write through SharedArrayBuffer", () => {
    test("element access works", () => {
      const sab = new SharedArrayBuffer(8);
      const ta = new Int32Array(sab);
      ta[0] = 42;
      ta[1] = -7;
      expect(ta[0]).toBe(42);
      expect(ta[1]).toBe(-7);
    });

    test("multiple views share the same memory", () => {
      const sab = new SharedArrayBuffer(4);
      const u8 = new Uint8Array(sab);
      const i32 = new Int32Array(sab);

      i32[0] = 0x01020304;
      expect(u8[0]).toBe(4);
      expect(u8[1]).toBe(3);
      expect(u8[2]).toBe(2);
      expect(u8[3]).toBe(1);
    });

    test("views with different offsets share memory", () => {
      const sab = new SharedArrayBuffer(8);
      const full = new Uint8Array(sab);
      const half = new Uint8Array(sab, 4);

      full[4] = 99;
      expect(half[0]).toBe(99);

      half[1] = 77;
      expect(full[5]).toBe(77);
    });
  });

  describe("all typed array kinds", () => {
    const kinds = [
      { Ctor: Int8Array, name: "Int8Array", bpe: 1 },
      { Ctor: Uint8Array, name: "Uint8Array", bpe: 1 },
      { Ctor: Uint8ClampedArray, name: "Uint8ClampedArray", bpe: 1 },
      { Ctor: Int16Array, name: "Int16Array", bpe: 2 },
      { Ctor: Uint16Array, name: "Uint16Array", bpe: 2 },
      { Ctor: Int32Array, name: "Int32Array", bpe: 4 },
      { Ctor: Uint32Array, name: "Uint32Array", bpe: 4 },
      { Ctor: Float32Array, name: "Float32Array", bpe: 4 },
      { Ctor: Float64Array, name: "Float64Array", bpe: 8 },
    ];

    kinds.forEach(({ Ctor, name, bpe }) => {
      test(name + " can be constructed from SharedArrayBuffer", () => {
        const sab = new SharedArrayBuffer(bpe * 4);
        const ta = new Ctor(sab);
        expect(ta.length).toBe(4);
        expect(ta.buffer).toBe(sab);
        expect(ta.buffer instanceof SharedArrayBuffer).toBe(true);
      });
    });
  });

  describe("slice returns a new TypedArray with own ArrayBuffer", () => {
    test("sliced view does not share the SharedArrayBuffer", () => {
      const sab = new SharedArrayBuffer(16);
      const ta = new Int32Array(sab);
      ta[0] = 1;
      ta[1] = 2;
      ta[2] = 3;
      ta[3] = 4;

      const sliced = ta.slice(1, 3);
      expect(sliced.length).toBe(2);
      expect(sliced[0]).toBe(2);
      expect(sliced[1]).toBe(3);
      expect(sliced.buffer).not.toBe(sab);
    });
  });

  describe("subarray shares the SharedArrayBuffer", () => {
    test("subarray returns a view on the same SharedArrayBuffer", () => {
      const sab = new SharedArrayBuffer(16);
      const ta = new Int32Array(sab);
      ta[0] = 10;
      ta[1] = 20;
      ta[2] = 30;
      ta[3] = 40;

      const sub = ta.subarray(1, 3);
      expect(sub.length).toBe(2);
      expect(sub[0]).toBe(20);
      expect(sub[1]).toBe(30);
      expect(sub.buffer).toBe(sab);

      sub[0] = 99;
      expect(ta[1]).toBe(99);
    });
  });
});
