describe("Uint8Array.prototype.setFromHex", () => {
  test("decodes hex into existing array", () => {
    const target = new Uint8Array(5);
    const result = target.setFromHex("48656c6c6f");
    expect(result.read).toBe(10);
    expect(result.written).toBe(5);
    expect(target[0]).toBe(72);
    expect(target[4]).toBe(111);
  });

  test("returns { read, written } object", () => {
    const target = new Uint8Array(3);
    const result = target.setFromHex("414243");
    expect(result.read).toBe(6);
    expect(result.written).toBe(3);
  });

  test("stops writing when target is full", () => {
    const target = new Uint8Array(2);
    const result = target.setFromHex("414243");
    expect(result.written).toBe(2);
    expect(result.read).toBe(4);
    expect(target[0]).toBe(65);
    expect(target[1]).toBe(66);
  });

  test("handles empty input", () => {
    const target = new Uint8Array(5);
    const result = target.setFromHex("");
    expect(result.read).toBe(0);
    expect(result.written).toBe(0);
  });

  test("accepts uppercase hex", () => {
    const target = new Uint8Array(3);
    const result = target.setFromHex("AABBCC");
    expect(result.written).toBe(3);
    expect(target[0]).toBe(0xAA);
    expect(target[1]).toBe(0xBB);
    expect(target[2]).toBe(0xCC);
  });

  test("accepts mixed case hex", () => {
    const target = new Uint8Array(2);
    const result = target.setFromHex("aAbB");
    expect(target[0]).toBe(0xAA);
    expect(target[1]).toBe(0xBB);
  });

  test("throws TypeError when called on non-Uint8Array", () => {
    const fn = Uint8Array.prototype.setFromHex;
    expect(() => fn.call(new Int8Array(1), "ff")).toThrow(TypeError);
  });

  test("throws TypeError when first argument is not a string", () => {
    const target = new Uint8Array(5);
    expect(() => target.setFromHex(123)).toThrow(TypeError);
  });

  test("throws SyntaxError for odd-length hex string", () => {
    const target = new Uint8Array(5);
    expect(() => target.setFromHex("abc")).toThrow(SyntaxError);
  });

  test("throws SyntaxError for invalid hex characters", () => {
    const target = new Uint8Array(5);
    expect(() => target.setFromHex("zzzz")).toThrow(SyntaxError);
  });
});
