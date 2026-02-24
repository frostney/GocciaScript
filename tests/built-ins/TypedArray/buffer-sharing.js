describe("TypedArray buffer sharing", () => {
  describe("multiple views on same buffer", () => {
    test("writing through one view is visible through another", () => {
      const buf = new ArrayBuffer(4);
      const u8 = new Uint8Array(buf);
      const i32 = new Int32Array(buf);

      i32[0] = 0x01020304;
      expect(u8[0]).toBe(4);
      expect(u8[1]).toBe(3);
      expect(u8[2]).toBe(2);
      expect(u8[3]).toBe(1);
    });

    test("Uint16 and Uint8 views share data", () => {
      const buf = new ArrayBuffer(4);
      const u16 = new Uint16Array(buf);
      const u8 = new Uint8Array(buf);

      u16[0] = 0x0102;
      expect(u8[0]).toBe(2);
      expect(u8[1]).toBe(1);
    });

    test("overlapping subarray views", () => {
      const ta = new Int32Array([1, 2, 3, 4, 5]);
      const sub1 = ta.subarray(1, 4);
      const sub2 = ta.subarray(2, 5);

      sub1[0] = 99;
      expect(ta[1]).toBe(99);
      expect(sub2[0]).toBe(3);

      sub2[0] = 88;
      expect(ta[2]).toBe(88);
      expect(sub1[1]).toBe(88);
    });
  });

  describe("buffer property", () => {
    test("returns the backing ArrayBuffer", () => {
      const buf = new ArrayBuffer(8);
      const ta = new Int32Array(buf);
      expect(ta.buffer).toBe(buf);
    });

    test("auto-created buffer is accessible", () => {
      const ta = new Int32Array(4);
      expect(ta.buffer).toBeDefined();
      expect(ta.buffer.byteLength).toBe(16);
    });

    test("subarray shares the same buffer", () => {
      const ta = new Int32Array([1, 2, 3, 4]);
      const sub = ta.subarray(1);
      expect(sub.buffer).toBe(ta.buffer);
    });
  });

  describe("Object.prototype.toString.call()", () => {
    test("Int8Array tag", () => {
      expect(Object.prototype.toString.call(new Int8Array(0))).toBe("[object Int8Array]");
    });
    test("Uint8Array tag", () => {
      expect(Object.prototype.toString.call(new Uint8Array(0))).toBe("[object Uint8Array]");
    });
    test("Uint8ClampedArray tag", () => {
      expect(Object.prototype.toString.call(new Uint8ClampedArray(0))).toBe("[object Uint8ClampedArray]");
    });
    test("Int16Array tag", () => {
      expect(Object.prototype.toString.call(new Int16Array(0))).toBe("[object Int16Array]");
    });
    test("Uint16Array tag", () => {
      expect(Object.prototype.toString.call(new Uint16Array(0))).toBe("[object Uint16Array]");
    });
    test("Int32Array tag", () => {
      expect(Object.prototype.toString.call(new Int32Array(0))).toBe("[object Int32Array]");
    });
    test("Uint32Array tag", () => {
      expect(Object.prototype.toString.call(new Uint32Array(0))).toBe("[object Uint32Array]");
    });
    test("Float32Array tag", () => {
      expect(Object.prototype.toString.call(new Float32Array(0))).toBe("[object Float32Array]");
    });
    test("Float64Array tag", () => {
      expect(Object.prototype.toString.call(new Float64Array(0))).toBe("[object Float64Array]");
    });
  });
});
