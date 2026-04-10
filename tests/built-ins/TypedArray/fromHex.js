describe("Uint8Array.fromHex", () => {
  test("decodes hex string to Uint8Array", () => {
    const result = Uint8Array.fromHex("48656c6c6f");
    expect(result.length).toBe(5);
    expect(result[0]).toBe(72);
    expect(result[1]).toBe(101);
    expect(result[4]).toBe(111);
  });

  test("decodes empty string", () => {
    const result = Uint8Array.fromHex("");
    expect(result.length).toBe(0);
  });

  test("accepts lowercase hex", () => {
    const result = Uint8Array.fromHex("aabbcc");
    expect(result[0]).toBe(0xAA);
    expect(result[1]).toBe(0xBB);
    expect(result[2]).toBe(0xCC);
  });

  test("accepts uppercase hex", () => {
    const result = Uint8Array.fromHex("AABBCC");
    expect(result[0]).toBe(0xAA);
    expect(result[1]).toBe(0xBB);
    expect(result[2]).toBe(0xCC);
  });

  test("accepts mixed case hex", () => {
    const result = Uint8Array.fromHex("aAbBcC");
    expect(result[0]).toBe(0xAA);
    expect(result[1]).toBe(0xBB);
    expect(result[2]).toBe(0xCC);
  });

  test("decodes boundary values", () => {
    const result = Uint8Array.fromHex("007f80ff");
    expect(result[0]).toBe(0);
    expect(result[1]).toBe(127);
    expect(result[2]).toBe(128);
    expect(result[3]).toBe(255);
  });

  test("returns Uint8Array instance", () => {
    const result = Uint8Array.fromHex("ff");
    expect(result.length).toBe(1);
    expect(result[0]).toBe(255);
  });

  test("throws TypeError when argument is not a string", () => {
    expect(() => Uint8Array.fromHex(123)).toThrow(TypeError);
    expect(() => Uint8Array.fromHex(null)).toThrow(TypeError);
  });

  test("throws TypeError with no arguments", () => {
    expect(() => Uint8Array.fromHex()).toThrow(TypeError);
  });

  test("throws SyntaxError for odd-length string", () => {
    expect(() => Uint8Array.fromHex("a")).toThrow(SyntaxError);
    expect(() => Uint8Array.fromHex("abc")).toThrow(SyntaxError);
    expect(() => Uint8Array.fromHex("abcde")).toThrow(SyntaxError);
  });

  test("throws SyntaxError for invalid hex characters", () => {
    expect(() => Uint8Array.fromHex("zz")).toThrow(SyntaxError);
    expect(() => Uint8Array.fromHex("gg")).toThrow(SyntaxError);
    expect(() => Uint8Array.fromHex("0x")).toThrow(SyntaxError);
  });

  test("round-trips with toHex", () => {
    const original = new Uint8Array([0, 42, 128, 255]);
    const hex = original.toHex();
    const decoded = Uint8Array.fromHex(hex);
    expect(decoded.length).toBe(original.length);
    expect(decoded[0]).toBe(0);
    expect(decoded[1]).toBe(42);
    expect(decoded[2]).toBe(128);
    expect(decoded[3]).toBe(255);
  });
});
