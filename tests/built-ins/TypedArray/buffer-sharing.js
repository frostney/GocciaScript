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
});
