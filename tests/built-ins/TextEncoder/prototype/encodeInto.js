describe("TextEncoder.prototype.encodeInto", () => {
  const enc = new TextEncoder();

  test("encodes ASCII into a Uint8Array that is large enough", () => {
    const dest = new Uint8Array(10);
    const result = enc.encodeInto("Hello", dest);
    expect(result.read).toBe(5);
    expect(result.written).toBe(5);
    expect(dest[0]).toBe(72);  // H
    expect(dest[4]).toBe(111); // o
  });

  test("stops when destination is full (ASCII)", () => {
    const dest = new Uint8Array(3);
    const result = enc.encodeInto("ABCDE", dest);
    expect(result.read).toBe(3);
    expect(result.written).toBe(3);
    expect(dest[0]).toBe(65); // A
    expect(dest[2]).toBe(67); // C
  });

  test("does not split a multibyte sequence across the boundary", () => {
    // é is 2 bytes (C3 A9). A 1-byte destination cannot hold it.
    const dest = new Uint8Array(1);
    const result = enc.encodeInto("\u00E9", dest);
    expect(result.read).toBe(0);
    expect(result.written).toBe(0);
  });

  test("encodes a 2-byte character when space is available", () => {
    const dest = new Uint8Array(2);
    const result = enc.encodeInto("\u00E9", dest);
    expect(result.read).toBe(1);
    expect(result.written).toBe(2);
    expect(dest[0]).toBe(0xC3);
    expect(dest[1]).toBe(0xA9);
  });

  test("returns {read:0, written:0} for empty source", () => {
    const dest = new Uint8Array(10);
    const result = enc.encodeInto("", dest);
    expect(result.read).toBe(0);
    expect(result.written).toBe(0);
  });

  test("a 4-byte emoji counts as 2 UTF-16 code units in read", () => {
    const dest = new Uint8Array(4);
    // U+1F600 is encoded as a surrogate pair in UTF-16: \uD83D\uDE00 = 2 code units
    const result = enc.encodeInto("\uD83D\uDE00", dest);
    expect(result.read).toBe(2);
    expect(result.written).toBe(4);
  });

  test("converts an undefined source to the string \"undefined\"", () => {
    const dest = new Uint8Array(16);
    expect(enc.encodeInto(undefined, dest)).toEqual({ read: 9, written: 9 });
    expect(new TextDecoder().decode(dest.subarray(0, 9))).toBe("undefined");
  });

  test("result object has read and written properties", () => {
    const dest = new Uint8Array(5);
    const result = enc.encodeInto("abc", dest);
    expect(typeof result.read).toBe("number");
    expect(typeof result.written).toBe("number");
  });

  test("throws TypeError when destination is not a Uint8Array", () => {
    expect(() => enc.encodeInto("hi", new Int8Array(10))).toThrow(TypeError);
  });

  // Web IDL §3.2.26: destination is an [AllowShared] Uint8Array without
  // [AllowResizable], so a view of a resizable ArrayBuffer or a growable
  // SharedArrayBuffer is a TypeError, whatever has happened to the buffer
  // since it was created, and nothing is written.
  test("throws TypeError for a Uint8Array over a resizable ArrayBuffer", () => {
    const rab = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(() => enc.encodeInto("ok", new Uint8Array(rab))).toThrow(TypeError);
    expect(Array.from(new Uint8Array(rab))).toEqual([0, 0, 0, 0]);
  });

  test("throws TypeError for a fixed-length view of a resizable ArrayBuffer", () => {
    const rab = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(() => enc.encodeInto("ok", new Uint8Array(rab, 0, 2))).toThrow(TypeError);
    expect(Array.from(new Uint8Array(rab))).toEqual([0, 0, 0, 0]);
  });

  test("throws TypeError for a view after its resizable buffer grew", () => {
    const rab = new ArrayBuffer(2, { maxByteLength: 8 });
    const view = new Uint8Array(rab);
    rab.resize(8);
    expect(() => enc.encodeInto("abcdefgh", view)).toThrow(TypeError);
    expect(Array.from(new Uint8Array(rab))).toEqual([0, 0, 0, 0, 0, 0, 0, 0]);
  });

  test("throws TypeError for a view after its resizable buffer shrank", () => {
    const rab = new ArrayBuffer(8, { maxByteLength: 8 });
    const view = new Uint8Array(rab);
    rab.resize(4);
    expect(() => enc.encodeInto("abcdefgh", view)).toThrow(TypeError);
    expect(Array.from(view)).toEqual([0, 0, 0, 0]);
  });

  test("throws TypeError for a view out of bounds of its shrunk resizable buffer", () => {
    const rab = new ArrayBuffer(4, { maxByteLength: 8 });
    const view = new Uint8Array(rab, 2, 2);
    rab.resize(1);
    expect(() => enc.encodeInto("ok", view)).toThrow(TypeError);
  });

  test("throws TypeError for a view of a detached resizable buffer", () => {
    const rab = new ArrayBuffer(4, { maxByteLength: 8 });
    const view = new Uint8Array(rab);
    rab.transfer();
    expect(() => enc.encodeInto("ok", view)).toThrow(TypeError);
  });

  test("throws TypeError for a Uint8Array over a growable SharedArrayBuffer", () => {
    const gsab = new SharedArrayBuffer(4, { maxByteLength: 16 });
    expect(() => enc.encodeInto("ok", new Uint8Array(gsab))).toThrow(TypeError);
    expect(Array.from(new Uint8Array(gsab))).toEqual([0, 0, 0, 0]);
  });

  test("throws TypeError for a fixed-length view of a growable SharedArrayBuffer", () => {
    const gsab = new SharedArrayBuffer(4, { maxByteLength: 16 });
    expect(() => enc.encodeInto("ok", new Uint8Array(gsab, 0, 2))).toThrow(TypeError);
    expect(Array.from(new Uint8Array(gsab))).toEqual([0, 0, 0, 0]);
  });

  test("converts source before refusing a resizable destination", () => {
    const rab = new ArrayBuffer(4, { maxByteLength: 16 });
    let calls = 0;
    const source = { toString: () => { calls++; return "ok"; } };
    expect(() => enc.encodeInto(source, new Uint8Array(rab))).toThrow(TypeError);
    expect(calls).toBe(1);
  });

  test("writes into a Uint8Array over a fixed-length SharedArrayBuffer", () => {
    const sab = new SharedArrayBuffer(4);
    expect(enc.encodeInto("ok", new Uint8Array(sab))).toEqual({ read: 2, written: 2 });
    expect(Array.from(new Uint8Array(sab))).toEqual([111, 107, 0, 0]);
  });

  test("writes nothing into a view of a detached fixed-length buffer", () => {
    const buffer = new ArrayBuffer(4);
    const view = new Uint8Array(buffer);
    buffer.transfer();
    expect(enc.encodeInto("ok", view)).toEqual({ read: 0, written: 0 });
  });

  test("writes nothing when source's toString detaches the fixed-length destination", () => {
    const buffer = new ArrayBuffer(4);
    const view = new Uint8Array(buffer);
    const source = { toString: () => { buffer.transfer(); return "ok"; } };
    expect(enc.encodeInto(source, view)).toEqual({ read: 0, written: 0 });
  });

  test("throws TypeError for a view of an immutable ArrayBuffer and writes nothing", () => {
    const buffer = new ArrayBuffer(4).transferToImmutable();
    expect(() => enc.encodeInto("ok", new Uint8Array(buffer))).toThrow(TypeError);
    expect(Array.from(new Uint8Array(buffer))).toEqual([0, 0, 0, 0]);
  });

  test("throws TypeError for a Uint8ClampedArray, a DataView and an ArrayBuffer", () => {
    expect(() => enc.encodeInto("ok", new Uint8ClampedArray(4))).toThrow(TypeError);
    expect(() => enc.encodeInto("ok", new DataView(new ArrayBuffer(4)))).toThrow(TypeError);
    expect(() => enc.encodeInto("ok", new ArrayBuffer(4))).toThrow(TypeError);
  });
});
