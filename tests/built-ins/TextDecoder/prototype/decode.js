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

  test("non-fatal mode replaces 0xFF with U+FFFD", () => {
    // Per WHATWG Encoding §4.1: ill-formed bytes become U+FFFD.
    expect(dec.decode(new Uint8Array([0xFF]))).toBe("\uFFFD");
  });

  test("non-fatal mode replaces truncated 2-byte sequence with U+FFFD", () => {
    expect(dec.decode(new Uint8Array([0xC3]))).toBe("\uFFFD");
  });

  test("non-fatal mode replaces surrogate bytes (ED A0 80) with three U+FFFDs", () => {
    // ED is rejected at its second byte (A0 > 9F for the ED leading byte);
    // then A0 and 80 are orphaned continuation bytes → three replacements.
    // Matches WHATWG spec and browser behaviour.
    expect(dec.decode(new Uint8Array([0xED, 0xA0, 0x80]))).toBe("\uFFFD\uFFFD\uFFFD");
  });

  test("non-fatal mode preserves valid sequences between invalid bytes", () => {
    // 'H' (0x48) + invalid 0xFF + 'i' (0x69) → H + U+FFFD + i
    expect(dec.decode(new Uint8Array([0x48, 0xFF, 0x69]))).toBe("H\uFFFDi");
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

  test("fatal mode rejects overlong 3-byte sequence (E0 80 80)", () => {
    // E0 must be followed by A0-BF; 80 is overlong.
    const fatalDec = new TextDecoder("utf-8", { fatal: true });
    expect(() => fatalDec.decode(new Uint8Array([0xE0, 0x80, 0x80]))).toThrow(TypeError);
  });

  test("fatal mode rejects UTF-16 surrogate pair (ED A0 80 = U+D800)", () => {
    const fatalDec = new TextDecoder("utf-8", { fatal: true });
    expect(() => fatalDec.decode(new Uint8Array([0xED, 0xA0, 0x80]))).toThrow(TypeError);
  });

  test("fatal mode rejects overlong 4-byte sequence (F0 80 80 80)", () => {
    // F0 must be followed by 90-BF; 80 is overlong.
    const fatalDec = new TextDecoder("utf-8", { fatal: true });
    expect(() => fatalDec.decode(new Uint8Array([0xF0, 0x80, 0x80, 0x80]))).toThrow(TypeError);
  });

  test("fatal mode rejects code point above U+10FFFF (F4 90 80 80)", () => {
    // F4 must be followed by 80-8F; 90 exceeds U+10FFFF.
    const fatalDec = new TextDecoder("utf-8", { fatal: true });
    expect(() => fatalDec.decode(new Uint8Array([0xF4, 0x90, 0x80, 0x80]))).toThrow(TypeError);
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

  // Web IDL §3.2.26: decode's input is an AllowSharedBufferSource without
  // [AllowResizable], so a resizable ArrayBuffer, a growable
  // SharedArrayBuffer and every view of one are a TypeError, whatever has
  // happened to the buffer since it was created.
  const hiBang = (buffer) => {
    new Uint8Array(buffer).set([104, 105, 33, 33]);
    return buffer;
  };
  const resizable = () => hiBang(new ArrayBuffer(4, { maxByteLength: 16 }));
  const growable = () => hiBang(new SharedArrayBuffer(4, { maxByteLength: 16 }));

  test("throws TypeError for a Uint8Array over a resizable ArrayBuffer", () => {
    expect(() => dec.decode(new Uint8Array(resizable()))).toThrow(TypeError);
  });

  test("throws TypeError for a fixed-length view of a resizable ArrayBuffer", () => {
    expect(() => dec.decode(new Uint8Array(resizable(), 0, 2))).toThrow(TypeError);
  });

  test("throws TypeError for a resizable ArrayBuffer itself", () => {
    expect(() => dec.decode(resizable())).toThrow(TypeError);
  });

  test("throws TypeError for a DataView over a resizable ArrayBuffer", () => {
    expect(() => dec.decode(new DataView(resizable()))).toThrow(TypeError);
  });

  test("throws TypeError for a Uint16Array over a resizable ArrayBuffer", () => {
    expect(() => dec.decode(new Uint16Array(resizable()))).toThrow(TypeError);
  });

  test("throws TypeError for a view after its resizable buffer grew", () => {
    const rab = resizable();
    const view = new Uint8Array(rab);
    rab.resize(16);
    expect(() => dec.decode(view)).toThrow(TypeError);
  });

  test("throws TypeError for a view after its resizable buffer shrank", () => {
    const rab = resizable();
    const view = new Uint8Array(rab);
    rab.resize(2);
    expect(() => dec.decode(view)).toThrow(TypeError);
  });

  test("throws TypeError for a view out of bounds of its shrunk resizable buffer", () => {
    const rab = resizable();
    const view = new Uint8Array(rab, 2, 2);
    rab.resize(1);
    expect(() => dec.decode(view)).toThrow(TypeError);
  });

  test("throws TypeError for a view of a detached resizable buffer", () => {
    const rab = resizable();
    const view = new Uint8Array(rab);
    rab.transfer();
    expect(() => dec.decode(view)).toThrow(TypeError);
  });

  test("throws TypeError for a detached resizable buffer itself", () => {
    const rab = resizable();
    rab.transfer();
    expect(() => dec.decode(rab)).toThrow(TypeError);
  });

  test("throws TypeError for a Uint8Array over a growable SharedArrayBuffer", () => {
    expect(() => dec.decode(new Uint8Array(growable()))).toThrow(TypeError);
  });

  test("throws TypeError for a growable SharedArrayBuffer itself", () => {
    expect(() => dec.decode(growable())).toThrow(TypeError);
  });

  test("throws TypeError for a DataView over a growable SharedArrayBuffer", () => {
    expect(() => dec.decode(new DataView(growable()))).toThrow(TypeError);
  });

  test("throws TypeError for a view after its growable buffer grew", () => {
    const gsab = growable();
    const view = new Uint8Array(gsab);
    gsab.grow(8);
    expect(() => dec.decode(view)).toThrow(TypeError);
  });

  test("decodes a resizable buffer's bytes once copied to a fixed-length buffer", () => {
    expect(dec.decode(resizable().slice(0))).toBe("hi!!");
    expect(dec.decode(resizable().transferToFixedLength())).toBe("hi!!");
  });

  // The same conversion accepts every fixed-length member of the union.
  test("decodes a Uint8Array over a fixed-length ArrayBuffer", () => {
    expect(dec.decode(new Uint8Array(hiBang(new ArrayBuffer(4))))).toBe("hi!!");
  });

  test("decodes a fixed-length SharedArrayBuffer", () => {
    expect(dec.decode(hiBang(new SharedArrayBuffer(4)))).toBe("hi!!");
  });

  test("decodes a Uint8Array over a fixed-length SharedArrayBuffer", () => {
    expect(dec.decode(new Uint8Array(hiBang(new SharedArrayBuffer(4))))).toBe("hi!!");
  });

  test("decodes a DataView over a fixed-length ArrayBuffer", () => {
    expect(dec.decode(new DataView(hiBang(new ArrayBuffer(4))))).toBe("hi!!");
  });

  test("decodes the range a DataView covers", () => {
    expect(dec.decode(new DataView(hiBang(new ArrayBuffer(4)), 1, 2))).toBe("i!");
  });

  test("decodes a DataView over a fixed-length SharedArrayBuffer", () => {
    expect(dec.decode(new DataView(hiBang(new SharedArrayBuffer(4)), 0, 2))).toBe("hi");
  });

  test("decodes a view of a detached fixed-length buffer as an empty string", () => {
    const buffer = hiBang(new ArrayBuffer(4));
    const view = new Uint8Array(buffer);
    const dataView = new DataView(buffer);
    buffer.transfer();
    expect(dec.decode(view)).toBe("");
    expect(dec.decode(dataView)).toBe("");
    expect(dec.decode(buffer)).toBe("");
  });

  test("throws TypeError for null and plain objects", () => {
    expect(() => dec.decode(null)).toThrow(TypeError);
    expect(() => dec.decode({ byteLength: 4 })).toThrow(TypeError);
    expect(() => dec.decode([104, 105])).toThrow(TypeError);
  });
});
