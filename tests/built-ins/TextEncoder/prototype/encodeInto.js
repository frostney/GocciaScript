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

  test("result object has read and written properties", () => {
    const dest = new Uint8Array(5);
    const result = enc.encodeInto("abc", dest);
    expect(typeof result.read).toBe("number");
    expect(typeof result.written).toBe("number");
  });

  test("throws TypeError when destination is not a Uint8Array", () => {
    expect(() => enc.encodeInto("hi", new Int8Array(10))).toThrow(TypeError);
  });
});
