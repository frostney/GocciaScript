describe("TextDecoder.prototype.decode", () => {
  const dec = new TextDecoder();

  test("returns empty string when called with no arguments", () => {
    expect(dec.decode()).toBe("");
  });

  test("returns empty string for undefined input", () => {
    expect(dec.decode(undefined)).toBe("");
  });

  test("decodes an empty Uint8Array to an empty string", () => {
    expect(dec.decode(new Uint8Array(0))).toBe("");
  });

  test("decodes ASCII bytes from a Uint8Array", () => {
    const bytes = new Uint8Array([72, 101, 108, 108, 111]); // Hello
    expect(dec.decode(bytes)).toBe("Hello");
  });

  test("decodes a 2-byte UTF-8 sequence (é, U+00E9)", () => {
    const bytes = new Uint8Array([0xC3, 0xA9]);
    expect(dec.decode(bytes)).toBe("\u00E9");
  });

  test("decodes a 3-byte UTF-8 sequence (中, U+4E2D)", () => {
    const bytes = new Uint8Array([0xE4, 0xB8, 0xAD]);
    expect(dec.decode(bytes)).toBe("\u4E2D");
  });

  test("decodes a 4-byte UTF-8 sequence (😀, U+1F600)", () => {
    const bytes = new Uint8Array([0xF0, 0x9F, 0x98, 0x80]);
    expect(dec.decode(bytes)).toBe("\uD83D\uDE00");
  });

  test("decodes a mixed ASCII and multibyte string", () => {
    const bytes = new Uint8Array([97, 0xC3, 0xA9, 98]); // a + é + b
    expect(dec.decode(bytes)).toBe("a\u00E9b");
  });

  test("decodes an ArrayBuffer directly", () => {
    const buf = new ArrayBuffer(5);
    const view = new Uint8Array(buf);
    view[0] = 72; view[1] = 101; view[2] = 108; view[3] = 108; view[4] = 111;
    expect(dec.decode(buf)).toBe("Hello");
  });

  test("strips UTF-8 BOM (EF BB BF) by default", () => {
    const bytes = new Uint8Array([0xEF, 0xBB, 0xBF, 72, 105]); // BOM + Hi
    expect(dec.decode(bytes)).toBe("Hi");
  });

  test("preserves BOM when ignoreBOM is true", () => {
    const decIgnore = new TextDecoder("utf-8", { ignoreBOM: true });
    const bytes = new Uint8Array([0xEF, 0xBB, 0xBF, 72, 105]); // BOM + Hi
    // BOM decoded as U+FEFF
    expect(decIgnore.decode(bytes)).toBe("\uFEFFHi");
  });

  test("decodes a null byte (U+0000)", () => {
    const bytes = new Uint8Array([0x00]);
    expect(dec.decode(bytes)).toBe("\u0000");
  });

  test("non-fatal mode returns a string for invalid bytes (no throw)", () => {
    // Non-fatal mode — result may be garbled but must produce a string.
    const bytes = new Uint8Array([0xFF]);
    const result = dec.decode(bytes);
    expect(typeof result).toBe("string");
  });

  test("fatal mode throws TypeError for invalid UTF-8 byte sequence", () => {
    const fatalDec = new TextDecoder("utf-8", { fatal: true });
    const bad = new Uint8Array([0xFF]); // invalid leading byte
    expect(() => fatalDec.decode(bad)).toThrow(TypeError);
  });

  test("fatal mode throws TypeError for truncated multi-byte sequence", () => {
    const fatalDec = new TextDecoder("utf-8", { fatal: true });
    const bad = new Uint8Array([0xC3]); // 2-byte start with no continuation
    expect(() => fatalDec.decode(bad)).toThrow(TypeError);
  });

  test("fatal mode accepts valid UTF-8 without throwing", () => {
    const fatalDec = new TextDecoder("utf-8", { fatal: true });
    const bytes = new Uint8Array([72, 101, 108, 108, 111]);
    expect(fatalDec.decode(bytes)).toBe("Hello");
  });

  test("throws TypeError for non-buffer-source input", () => {
    expect(() => dec.decode("not a buffer")).toThrow(TypeError);
  });

  test("decodes a typed array backed by an offset into a shared buffer", () => {
    const buf = new ArrayBuffer(6);
    const view = new Uint8Array(buf);
    view[0] = 0; view[1] = 72; view[2] = 105; // 0, H, i
    const slice = new Uint8Array(buf, 1, 2); // H, i
    expect(dec.decode(slice)).toBe("Hi");
  });

  test("decodes Int8Array containing ASCII bytes", () => {
    const bytes = new Int8Array([72, 101, 108, 108, 111]); // Hello
    expect(dec.decode(bytes)).toBe("Hello");
  });
});
