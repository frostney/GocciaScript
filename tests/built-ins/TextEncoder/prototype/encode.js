describe("TextEncoder.prototype.encode", () => {
  const enc = new TextEncoder();

  test("encodes an empty string to an empty Uint8Array", () => {
    const result = enc.encode("");
    expect(result instanceof Uint8Array).toBe(true);
    expect(result.length).toBe(0);
  });

  test("returns a new Uint8Array on each call", () => {
    const a = enc.encode("hi");
    const b = enc.encode("hi");
    expect(a === b).toBe(false);
  });

  test("encodes ASCII characters as single bytes", () => {
    const result = enc.encode("ABC");
    expect(result.length).toBe(3);
    expect(result[0]).toBe(65);
    expect(result[1]).toBe(66);
    expect(result[2]).toBe(67);
  });

  test("encodes a simple ASCII string", () => {
    const result = enc.encode("Hello");
    expect(result.length).toBe(5);
    expect(result[0]).toBe(72);  // H
    expect(result[1]).toBe(101); // e
    expect(result[2]).toBe(108); // l
    expect(result[3]).toBe(108); // l
    expect(result[4]).toBe(111); // o
  });

  test("encodes a 2-byte UTF-8 character (U+00E9, é)", () => {
    const result = enc.encode("\u00E9");
    expect(result.length).toBe(2);
    expect(result[0]).toBe(0xC3);
    expect(result[1]).toBe(0xA9);
  });

  test("encodes a 3-byte UTF-8 character (U+4E2D, 中)", () => {
    const result = enc.encode("\u4E2D");
    expect(result.length).toBe(3);
    expect(result[0]).toBe(0xE4);
    expect(result[1]).toBe(0xB8);
    expect(result[2]).toBe(0xAD);
  });

  test("encodes a 4-byte UTF-8 character (U+1F600, 😀)", () => {
    const result = enc.encode("\uD83D\uDE00");
    expect(result.length).toBe(4);
    expect(result[0]).toBe(0xF0);
    expect(result[1]).toBe(0x9F);
    expect(result[2]).toBe(0x98);
    expect(result[3]).toBe(0x80);
  });

  test("encodes a mixed ASCII and multibyte string", () => {
    const result = enc.encode("a\u00E9b");
    // a = 1 byte, é = 2 bytes, b = 1 byte → total 4
    expect(result.length).toBe(4);
    expect(result[0]).toBe(97);   // a
    expect(result[1]).toBe(0xC3); // é high
    expect(result[2]).toBe(0xA9); // é low
    expect(result[3]).toBe(98);   // b
  });

  test("undefined input is treated as empty string", () => {
    const result = enc.encode(undefined);
    expect(result.length).toBe(0);
  });

  test("encodes a null byte (U+0000)", () => {
    const result = enc.encode("\u0000");
    expect(result.length).toBe(1);
    expect(result[0]).toBe(0);
  });

  test("encodes U+0080 as a 2-byte sequence", () => {
    const result = enc.encode("\u0080");
    expect(result.length).toBe(2);
    expect(result[0]).toBe(0xC2);
    expect(result[1]).toBe(0x80);
  });

  test("lone high surrogate (\\uD800) is encoded as U+FFFD (EF BF BD)", () => {
    // Per WHATWG Encoding §8.3.2: TextEncoder normalises lone surrogates to U+FFFD.
    const result = enc.encode("\uD800");
    expect(result.length).toBe(3);
    expect(result[0]).toBe(0xEF);
    expect(result[1]).toBe(0xBF);
    expect(result[2]).toBe(0xBD);
  });

  test("lone low surrogate (\\uDFFF) is encoded as U+FFFD (EF BF BD)", () => {
    const result = enc.encode("\uDFFF");
    expect(result.length).toBe(3);
    expect(result[0]).toBe(0xEF);
    expect(result[1]).toBe(0xBF);
    expect(result[2]).toBe(0xBD);
  });

  test("throws TypeError when called on a non-TextEncoder receiver", () => {
    expect(() => TextEncoder.prototype.encode.call({}, "x")).toThrow(TypeError);
  });
});
