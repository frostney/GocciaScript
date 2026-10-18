describe("Uint8Array.prototype.toHex", () => {
  test("has built-in method attributes", () => {
    expect(Object.getOwnPropertyDescriptor(Uint8Array.prototype, "toHex")).toEqual({
      value: Uint8Array.prototype.toHex,
      writable: true,
      enumerable: false,
      configurable: true,
    });
  });

  test("encodes bytes as lowercase hex pairs", () => {
    const bytes = new Uint8Array([0x48, 0x65, 0x6c, 0x6c, 0x6f]);
    expect(bytes.toHex()).toBe("48656c6c6f");
  });

  test("encodes single byte", () => {
    expect(new Uint8Array([0xff]).toHex()).toBe("ff");
    expect(new Uint8Array([0x00]).toHex()).toBe("00");
    expect(new Uint8Array([0x0a]).toHex()).toBe("0a");
  });

  test("returns empty string for empty array", () => {
    expect(new Uint8Array(0).toHex()).toBe("");
  });

  test("always produces lowercase hex", () => {
    const bytes = new Uint8Array([0xAB, 0xCD, 0xEF]);
    expect(bytes.toHex()).toBe("abcdef");
  });

  test("encodes all byte values", () => {
    const bytes = new Uint8Array([0, 127, 128, 255]);
    expect(bytes.toHex()).toBe("007f80ff");
  });

  test("throws TypeError when called on non-Uint8Array", () => {
    const fn = Uint8Array.prototype.toHex;
    expect(() => fn.call(new Int8Array(1))).toThrow(TypeError);
    expect(() => fn.call(new Int32Array(1))).toThrow(TypeError);
  });

  test("throws TypeError on detached buffers", () => {
    const buffer = new ArrayBuffer(2);
    const bytes = new Uint8Array(buffer);
    buffer.transfer();

    expect(() => bytes.toHex()).toThrow(TypeError);
  });

  test("works with buffer offset", () => {
    const buffer = new ArrayBuffer(8);
    const full = new Uint8Array(buffer);
    full[2] = 0xAA;
    full[3] = 0xBB;
    full[4] = 0xCC;
    const view = new Uint8Array(buffer, 2, 3);
    expect(view.toHex()).toBe("aabbcc");
  });

  test("encodes a length-tracking view after its buffer grew, with zeros past the old end", () => {
    const rab = new ArrayBuffer(2, { maxByteLength: 8 });
    const view = new Uint8Array(rab);
    view.fill(255);
    rab.resize(8);
    expect(view.toHex()).toBe("ffff000000000000");
  });

  test("encodes a length-tracking view after its buffer shrank", () => {
    const rab = new ArrayBuffer(8, { maxByteLength: 8 });
    const view = new Uint8Array(rab);
    view.fill(9);
    rab.resize(3);
    expect(view.toHex()).toBe("090909");
  });

  test("throws TypeError for a view past the end of its shrunk buffer", () => {
    const rab = new ArrayBuffer(8, { maxByteLength: 8 });
    const view = new Uint8Array(rab, 0, 8);
    rab.resize(3);
    expect(() => view.toHex()).toThrow(TypeError);
  });
});
